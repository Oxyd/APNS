#include "tree.hpp"

#include <gtest/gtest.h>

#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/ref.hpp>

#include <queue>

namespace {

boost::shared_ptr<vertex> make_tree() {
  boost::shared_ptr<vertex> root(new vertex);
  root->proof_number = 1;
  root->disproof_number = 1;

  vertex::children_iterator a = root->add_child();
  a->proof_number = 1;
  a->disproof_number = 2;

  vertex::children_iterator aa = a->add_child();
  aa->proof_number = 1;
  aa->disproof_number = 3;

  vertex::children_iterator ab = a->add_child();
  ab->proof_number = 2;
  ab->disproof_number = 3;

  vertex::children_iterator b = root->add_child();
  b->proof_number = 2;
  b->disproof_number = 2;

  vertex::children_iterator ba = root->add_child();
  ba->proof_number = 1;
  ba->disproof_number = 3;

  vertex::children_iterator bb = root->add_child();
  bb->proof_number = 2;
  bb->disproof_number = 3;

  vertex::children_iterator bc = root->add_child();
  bc->proof_number = 3;
  bc->disproof_number = 3;

  return root;
}

struct bfs_traversal {
  vertex::children_iterator operator () (vertex& current) {
    for (vertex::children_iterator child = current.children_begin(); child != current.children_end(); ++child)
      queue.push(child);

    if (!queue.empty()) {
      vertex::children_iterator n = queue.front();
      queue.pop();
      return n;
    } else return 0;
  }

private:
  std::queue<vertex::children_iterator> queue;
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
  for (vertex::const_children_iterator child = v.children_begin();
       child != v.children_end();
       ++child)
    EXPECT_GE(child->children_count(), 0);
}

} // anonymous namespace

TEST(vertex, children_insertion_test) {
  vertex v;
  v.add_child();
  v.add_child();
  v.add_child();

  EXPECT_EQ(3, v.children_count());
  verify_iterable(v);
}

TEST(vertex, resize_test) {
  vertex v;
  v.resize(3);

  EXPECT_EQ(3, v.children_count());
  verify_iterable(v);
}

TEST(vertex, reserve_test) {
  vertex v;
  v.reserve(3);

  EXPECT_EQ(0, v.children_count());
  EXPECT_TRUE(v.children_begin() == v.children_end());

  v.add_child();
  v.add_child();

  EXPECT_EQ(2, v.children_count());
  verify_iterable(v);
}

TEST(vertex, children_removal_test) {
  vertex v;

  v.add_child()->proof_number = 1;
  v.add_child()->proof_number = 2;
  v.add_child()->proof_number = 10;
  v.add_child()->proof_number = 3;
  v.add_child()->proof_number = 4;

  EXPECT_EQ(5, v.children_count());
  verify_iterable(v);

  vertex::children_iterator child = v.children_begin();
  for (; child != v.children_end(); ++child)
    if (child->proof_number == 10) break;

  v.remove_child(child);

  EXPECT_EQ(4, v.children_count());
  
  vertex::number_t n = 0;
  for (vertex::children_iterator child = v.children_begin();
       child != v.children_end();
       ++child) {
    EXPECT_EQ(n + 1, child->proof_number);
    n = child->proof_number;
  }
}

TEST(vertex, resize_down_test) {
  vertex v;

  v.add_child()->proof_number = 1;
  v.add_child()->proof_number = 2;
  v.add_child()->proof_number = 3;
  v.add_child()->proof_number = 4;

  EXPECT_EQ(4, v.children_count());

  v.resize(2);

  EXPECT_EQ(2, v.children_count());

  vertex::number_t n = 0;
  for (vertex::children_iterator child = v.children_begin();
       child != v.children_end();
       ++child) {
    EXPECT_EQ(n + 1, child->proof_number);
    n = child->proof_number;
  }

  EXPECT_EQ(2, n);
}

TEST(vertex, resize_to_zero_test) {
  vertex v;

  vertex::children_iterator it = v.add_child();
  v.remove_child(it);

  EXPECT_EQ(0, v.children_count());
  EXPECT_TRUE(v.children_begin() == v.children_end());
}

TEST(vertex, pack_test) {
  vertex v;

  v.add_child()->proof_number = 1;
  v.add_child()->proof_number = 2;
  v.add_child()->proof_number = 3;
  v.add_child()->proof_number = 4;

  EXPECT_EQ(4, v.children_count());

  v.resize(2);
  v.pack();

  EXPECT_EQ(2, v.children_count());

  vertex::number_t n = 0;
  for (vertex::children_iterator child = v.children_begin();
       child != v.children_end();
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

bool find_two(vertex& v) {
  return v.proof_number == 2;
}

TEST(traverser, stop_condition) {
  boost::shared_ptr<vertex> tree = make_tree();
  vertex::children_iterator found = traverse(*tree, bfs_traversal(), null_visitor(), &find_two);
  EXPECT_EQ(2, found->proof_number);
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

