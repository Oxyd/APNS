#include "search-algos.hpp"

#include <gtest/gtest.h>

#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>

using namespace apns;

namespace {

boost::shared_ptr<game> make_game(board& best_board) {
  board initial_board;
  initial_board.put(position(2, 'd'), piece(piece::gold, piece::elephant));
  initial_board.put(position(8, 'e'), piece(piece::silver, piece::elephant));

  boost::shared_ptr<game> g = boost::make_shared<game>(initial_board, piece::gold);

  g->root.disproof_number = 4;

  vertex::children_iterator child = g->root.add_child();
  child->step = step::validate_ordinary_step(initial_board, elementary_step::displacement(position(2, 'd'), north));
  child->proof_number = 1;
  child->disproof_number = 5;
  child->steps_remaining = 3;
  child->type = vertex::type_or;

  board b = initial_board;
  apply(*child->step, b);

  child = child->add_child();
  child->step = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'd'), north));
  child->proof_number = 1;
  child->disproof_number = 6;
  child->steps_remaining = 2;
  child->type = vertex::type_or;

  apply(*child->step, b);

  child = child->add_child();
  child->step = step::validate_ordinary_step(b, elementary_step::displacement(position(4, 'd'), west));
  child->proof_number = 1;
  child->disproof_number = 7;
  child->steps_remaining = 4;
  child->type = vertex::type_and;

  apply(*child->step, b);
  best_board = b;

  child = g->root.add_child();
  child->step = step::validate_ordinary_step(initial_board, elementary_step::displacement(position(2, 'd'), east));
  child->proof_number = 3;
  child->disproof_number = 2;
  child->steps_remaining = 3;
  child->type = vertex::type_or;

  return g;
}

boost::shared_ptr<game> make_game() {
  board b;
  return make_game(b);
}

} // anonymous namespace

TEST(traversing, find_best_vertex_test) {
  boost::shared_ptr<game> g = make_game();
  vertex* found = traverse(g->root, &best_successor);

  ASSERT_TRUE(found);
  EXPECT_EQ(1, found->proof_number);
  EXPECT_EQ(7, found->disproof_number);
  EXPECT_EQ(4, found->steps_remaining);
  EXPECT_EQ(vertex::type_and, found->type);
}

TEST(tree, empty_vertex_test) {
  vertex v;
  EXPECT_EQ(0, v.children_count());
}

TEST(tree, one_child_test) {
  vertex v;
  v.add_child()->proof_number = 1;

  EXPECT_EQ(1, v.children_count());

  std::size_t iterations = 0;
  for (vertex::children_iterator child = v.children_begin(); child != v.children_end(); ++child) {
    ++iterations;
    EXPECT_EQ(1, child->proof_number);
  }

  EXPECT_EQ(1, iterations);
}

TEST(tree, two_children_test) {
  vertex v;
  v.add_child()->proof_number = 1;
  v.add_child()->proof_number = 2;

  std::size_t iterations = 0;
  for (vertex::children_iterator child = v.children_begin(); child != v.children_end(); ++child) {
    ++iterations;
    EXPECT_EQ(iterations, child->proof_number);
  }

  EXPECT_EQ(2, iterations);
}

TEST(tree, three_children_test) {
  vertex v;
  v.add_child()->proof_number = 1;
  v.add_child()->proof_number = 2;
  v.add_child()->proof_number = 3;

  std::size_t iterations = 0;
  for (vertex::children_iterator child = v.children_begin(); child != v.children_end(); ++child) {
    ++iterations;
    EXPECT_EQ(iterations, child->proof_number);
  }

  EXPECT_EQ(3, iterations);
}

TEST(algorithm, update_numbers_test) {
  vertex v;
  v.proof_number = 1;
  v.disproof_number = 1;
  v.type = vertex::type_or;

  vertex::children_iterator child = v.add_child();
  child->proof_number = 3;
  child->disproof_number = 5;
  child->type = vertex::type_or;

  child = v.add_child();
  child->proof_number = 8;
  child->disproof_number = 2;
  child->type = vertex::type_or;

  child = v.add_child();
  child->proof_number = 5;
  child->disproof_number = 3;
  child->type = vertex::type_and;

  update_numbers(v);
  EXPECT_EQ(3, v.proof_number);
  EXPECT_EQ(10, v.disproof_number);

  v.type = vertex::type_and;
  update_numbers(v);
  EXPECT_EQ(16, v.proof_number);
  EXPECT_EQ(2, v.disproof_number);
}

TEST(winner_test, attacker_goal) {
  board b;
  b.put(position(8, 'd'), piece(piece::gold, piece::rabbit));
  b.put(position(4, 'h'), piece(piece::silver, piece::rabbit));

  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::gold);
}

TEST(winner_test, defender_goal) {
  board b;
  b.put(position(4, 'd'), piece(piece::gold, piece::rabbit));
  b.put(position(1, 'h'), piece(piece::silver, piece::rabbit));

  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::silver);
}

TEST(winner_test, attacker_out_of_rabbits) {
  board b;
  b.put(position(4, 'h'), piece(piece::silver, piece::rabbit));

  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::silver);
}

TEST(winner_test, defender_out_of_rabbits) {
  board b;
  b.put(position(4, 'h'), piece(piece::gold, piece::rabbit));

  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::gold);
}

TEST(history_stack_test, push_pop_test) {
  board b;
  b.put(position(1, 'a'), piece(piece::gold, piece::dog));
  b.put(position(8, 'h'), piece(piece::silver, piece::cat));

  zobrist_hasher hasher;
  zobrist_hasher::hash_t first_hash = hasher.generate_initial(b, piece::gold);

  vertex first;
  first.type = vertex::type_or;
  first.step = step::validate_ordinary_step(b, elementary_step::displacement(position(1, 'a'), north));

  apply(*first.step, b);

  vertex second;
  second.type = vertex::type_or;
  second.step = step::validate_ordinary_step(b, elementary_step::displacement(position(2, 'a'), north));

  apply(*second.step, b);

  vertex third;
  third.type = vertex::type_and;
  third.step = step::validate_ordinary_step(b, elementary_step::displacement(position(8, 'h'), south));

  apply(*third.step, b);

  zobrist_hasher::hash_t second_hash = hasher.generate_initial(b, piece::silver);

  vertex fourth;
  fourth.type = vertex::type_and;
  fourth.step = step::validate_ordinary_step(b, elementary_step::displacement(position(7, 'h'), south));

  history_stack h;

  unapply(*third.step, b);
  unapply(*second.step, b);
  unapply(*first.step, b);

  h.push(first, first_hash);
  ASSERT_EQ(1, h.records().size());
  EXPECT_EQ(first_hash, h.records().back().hash);

  apply(*first.step, b);

  h.push(second, 0);
  EXPECT_EQ(1, h.records().size());

  apply(*second.step, b);
  h.push(third, second_hash);

  EXPECT_EQ(2, h.records().size());
  EXPECT_EQ(second_hash, h.records().back().hash);

  apply(*third.step, b);
  h.push(fourth, 0);

  EXPECT_EQ(2, h.records().size());

  h.pop(fourth);
  EXPECT_EQ(2, h.records().size());

  h.pop(third);
  EXPECT_EQ(1, h.records().size());

  h.pop(second);
  EXPECT_EQ(1, h.records().size());

  h.pop(first);
  EXPECT_EQ(0, h.records().size());
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

