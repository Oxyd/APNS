#include "tree.hpp"

#include <gtest/gtest.h>

#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/ref.hpp>
#include <boost/bind.hpp>

#include <queue>

using namespace apns;

namespace {

boost::shared_ptr<vertex> make_tree() {
  boost::shared_ptr<vertex> root(new vertex);
  root->proof_number = 1;
  root->disproof_number = 1;

  vertex::iterator a = root->add();
  a->proof_number = 1;
  a->disproof_number = 2;

  vertex::iterator aa = a->add();
  aa->proof_number = 1;
  aa->disproof_number = 3;

  vertex::iterator ab = a->add();
  ab->proof_number = 2;
  ab->disproof_number = 3;

  vertex::iterator b = root->add();
  b->proof_number = 2;
  b->disproof_number = 2;

  vertex::iterator ba = root->add();
  ba->proof_number = 1;
  ba->disproof_number = 3;

  vertex::iterator bb = root->add();
  bb->proof_number = 2;
  bb->disproof_number = 3;

  vertex::iterator bc = root->add();
  bc->proof_number = 3;
  bc->disproof_number = 3;

  return root;
}

struct bfs_traversal {
  vertex const* operator () (vertex const& current) {
    for (vertex::const_iterator child = current.begin();
         child != current.end(); ++child)
      queue.push(child);

    if (!queue.empty()) {
      vertex::const_iterator n = queue.front();
      queue.pop();
      return &*n;
    } else 
      return 0;
  }

private:
  std::queue<vertex::const_iterator> queue;
};

struct bfs_checker_visitor {
  bfs_checker_visitor() :
    last_proof_n(0),
    last_disproof_n(0)
  { }

  void operator () (vertex const& n) {
    EXPECT_TRUE(n.proof_number == last_proof_n + 1 
                || (n.proof_number == 1
                    && (n.disproof_number == last_disproof_n + 1 || n.disproof_number == last_disproof_n)));
    last_proof_n    = n.proof_number;
    last_disproof_n = n.disproof_number;
  }

private:
  vertex::number_t last_proof_n;
  vertex::number_t last_disproof_n;
};

struct summing_visitor {
  summing_visitor() : 
    sum(0) 
  { }

  void operator () (vertex const& n) {
    sum += n.proof_number;
  }

  int sum;
};

void simple_visitor(vertex& n) {
  EXPECT_GE(n.proof_number, 1);
  EXPECT_LE(n.proof_number, 3);
}

struct poly_visitor : virtual_visitor::base {
  virtual void visit(vertex& n) {
    EXPECT_GE(n.proof_number, 1);
    EXPECT_LE(n.proof_number, 3);
  }
};

void verify_iterable(vertex const& v) {
  for (vertex::const_iterator child = v.begin();
       child != v.end();
       ++child)
    EXPECT_GE(child->size(), 0);
}

} // anonymous namespace

TEST(vertex, children_insertion_test) {
  vertex v;
  v.add();
  v.add();
  v.add();

  EXPECT_EQ(3, v.size());
  verify_iterable(v);
}

TEST(vertex, resize_test) {
  vertex v;
  v.resize(3);

  EXPECT_EQ(3, v.size());
  verify_iterable(v);
}

TEST(vertex, reserve_test) {
  vertex v;
  v.reserve(3);

  EXPECT_EQ(0, v.size());
  EXPECT_TRUE(v.begin() == v.end());

  v.add();
  v.add();

  EXPECT_EQ(2, v.size());
  verify_iterable(v);
}

TEST(vertex, children_removal_test) {
  vertex v;

  v.add()->proof_number = 1;
  v.add()->proof_number = 2;
  v.add()->proof_number = 10;
  v.add()->proof_number = 3;
  v.add()->proof_number = 4;

  EXPECT_EQ(5, v.size());
  verify_iterable(v);

  vertex::iterator child = v.begin();
  for (; child != v.end(); ++child)
    if (child->proof_number == 10) break;

  v.remove(child);

  EXPECT_EQ(4, v.size());
  
  vertex::number_t n = 0;
  for (vertex::iterator child = v.begin();
       child != v.end();
       ++child) {
    EXPECT_EQ(n + 1, child->proof_number);
    n = child->proof_number;
  }
}

TEST(vertex, resize_down_test) {
  vertex v;

  v.add()->proof_number = 1;
  v.add()->proof_number = 2;
  v.add()->proof_number = 3;
  v.add()->proof_number = 4;

  EXPECT_EQ(4, v.size());

  v.resize(2);

  EXPECT_EQ(2, v.size());

  vertex::number_t n = 0;
  for (vertex::iterator child = v.begin();
       child != v.end();
       ++child) {
    EXPECT_EQ(n + 1, child->proof_number);
    n = child->proof_number;
  }

  EXPECT_EQ(2, n);
}

TEST(vertex, resize_to_zero_test) {
  vertex v;

  vertex::iterator it = v.add();
  v.remove(it);

  EXPECT_EQ(0, v.size());
  EXPECT_TRUE(v.begin() == v.end());
}

TEST(vertex, pack_test) {
  vertex v;

  v.add()->proof_number = 1;
  v.add()->proof_number = 2;
  v.add()->proof_number = 3;
  v.add()->proof_number = 4;

  EXPECT_EQ(4, v.size());

  v.resize(2);
  v.pack();

  EXPECT_EQ(2, v.size());

  vertex::number_t n = 0;
  for (vertex::iterator child = v.begin();
       child != v.end();
       ++child) {
    EXPECT_EQ(n + 1, child->proof_number);
    n = child->proof_number;
  }

  EXPECT_EQ(2, n);
}

// Basic test of tree traversal.
TEST(traverser, traversal_test) {
  boost::shared_ptr<vertex> tree = make_tree();
  traverse(*tree, bfs_traversal(), bfs_checker_visitor());
}

// Test whether stateful visitors work and that we can query their state after the traversal.
TEST(traverser, stateful_visiting_test) {
  boost::shared_ptr<vertex> tree = make_tree();
  summing_visitor sum;
  traverse(*tree, bfs_traversal(), boost::ref(sum));
  EXPECT_EQ(13, sum.sum);
}

// Test that ordinary functions can be used as visitors too.
TEST(traverser, simple_function_visitor_test) {
  boost::shared_ptr<vertex> tree = make_tree();
  traverse(*tree, bfs_traversal(), &simple_visitor);
}

TEST(traverser, two_visitors) {
  boost::shared_ptr<vertex> tree = make_tree();
  summing_visitor s;
  traverse(*tree, bfs_traversal(),
           make_composite_visitor(bfs_checker_visitor(), boost::ref(s)));
  EXPECT_EQ(13, s.sum);
}

TEST(traverser, three_visitors) {
  boost::shared_ptr<vertex> tree = make_tree();
  summing_visitor s;
  traverse(
    *tree,
    bfs_traversal(),
    make_composite_visitor(
      bfs_checker_visitor(),
      make_composite_visitor(
        boost::ref(s),
        &simple_visitor
      )
    )
  );
  EXPECT_EQ(13, s.sum);
}

TEST(traverser, virtual_visitor) {
  boost::shared_ptr<vertex> tree = make_tree();
  traverse(*tree, bfs_traversal(), virtual_visitor(boost::make_shared<poly_visitor>()));
}

TEST(traverser, backtrack_test) {
  boost::shared_ptr<vertex> tree = make_tree();
  vertex_counter counter;
  traverse(*tree, backtrack(), boost::ref(counter));

  EXPECT_EQ(8, counter.count);
}

TEST(general, swap_test) {
  vertex parent;
  parent.reserve(2);

  vertex::iterator x = parent.add();
  x->proof_number = 1;
  x->disproof_number = 2;
  x->steps_remaining = 3;
  x->step = step::from_string("Dc3n");

  vertex::iterator y = parent.add();
  y->proof_number = 10;
  y->disproof_number = 11;
  y->steps_remaining = 2;
  y->step = step::from_string("rd2e Ce5w");

  x = parent.begin();

  parent.swap_children(x, y);

  EXPECT_EQ(10, x->proof_number);
  EXPECT_EQ(11, x->disproof_number);
  EXPECT_EQ(2, x->steps_remaining);
  ASSERT_TRUE(x->step);
  EXPECT_EQ("rd2e Ce5w", x->step->to_string());

  EXPECT_EQ(1, y->proof_number);
  EXPECT_EQ(2, y->disproof_number);
  EXPECT_EQ(3, y->steps_remaining);
  ASSERT_TRUE(y->step);
  EXPECT_EQ("Dc3n", y->step->to_string());
}

namespace {

void expect_sorted(vertex const& v) {
  vertex::number_t last = 0;
  for (vertex::const_iterator c = v.begin(); c != v.end(); ++c) {
    EXPECT_LE(last, c->proof_number);
    last = c->proof_number;
  }
}

} // anonymous namespace

TEST(general, sort_test) {
  vertex v;
  v.add()->proof_number = 5;
  v.add()->proof_number = 1;
  v.add()->proof_number = 8;
  v.add()->proof_number = 3;
  v.add()->proof_number = 9;
  v.add()->proof_number = 3;

  sort_children(v, boost::bind(&vertex::proof_number, _1) < boost::bind(&vertex::proof_number, _2));
  EXPECT_EQ(6, v.size());

  expect_sorted(v);
}

bool find_two(vertex& v) {
  return v.proof_number == 2;
}

TEST(traverser, stop_condition) {
  boost::shared_ptr<vertex> tree = make_tree();
  vertex* found = traverse(*tree, bfs_traversal(), null_visitor(), &find_two);
  EXPECT_EQ(2, found->proof_number);
}

TEST(traverser, backtracking) {
  boost::shared_ptr<vertex> tree = make_tree();
  vertex_counter counter;
  traverse(*tree, backtrack(), boost::ref(counter));

  EXPECT_EQ(8, counter.count);
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

