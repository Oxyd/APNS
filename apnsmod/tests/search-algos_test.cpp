#include "search-algos.hpp"

#include <gtest/gtest.h>

#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/bind.hpp>

using namespace apns;

namespace {

boost::shared_ptr<game> make_game(board& best_board) {
  board initial_board;
  initial_board.put(position(2, 'd'), piece(piece::gold, piece::elephant));
  initial_board.put(position(8, 'e'), piece(piece::silver, piece::elephant));

  boost::shared_ptr<game> g = boost::make_shared<game>(initial_board, piece::gold);

  g->root.disproof_number = 4;

  vertex::iterator child = g->root.add();
  child->step = step::validate_ordinary_step(initial_board, elementary_step::displacement(position(2, 'd'), north));
  child->proof_number = 1;
  child->disproof_number = 5;
  child->steps_remaining = 3;
  child->type = vertex::type_or;

  board b = initial_board;
  apply(*child->step, b);

  child = child->add();
  child->step = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'd'), north));
  child->proof_number = 1;
  child->disproof_number = 6;
  child->steps_remaining = 2;
  child->type = vertex::type_or;

  apply(*child->step, b);

  child = child->add();
  child->step = step::validate_ordinary_step(b, elementary_step::displacement(position(4, 'd'), west));
  child->proof_number = 1;
  child->disproof_number = 7;
  child->steps_remaining = 4;
  child->type = vertex::type_and;

  apply(*child->step, b);
  best_board = b;

  child = g->root.add();
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
  vertex* found = traverse(g->root, static_cast<fun_tp>(&best_successor));

  ASSERT_TRUE(found);
  EXPECT_EQ(1, found->proof_number);
  EXPECT_EQ(7, found->disproof_number);
  EXPECT_EQ(4, found->steps_remaining);
  EXPECT_EQ(vertex::type_and, found->type);
}

TEST(algorithm, update_numbers_test) {
  vertex v;
  v.proof_number = 1;
  v.disproof_number = 1;
  v.type = vertex::type_or;

  vertex::iterator child = v.add();
  child->proof_number = 3;
  child->disproof_number = 5;
  child->type = vertex::type_or;

  child = v.add();
  child->proof_number = 8;
  child->disproof_number = 2;
  child->type = vertex::type_or;

  child = v.add();
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

TEST(winner_test, gold_goal) {
  board b;
  b.put(position(8, 'd'), piece(piece::gold, piece::rabbit));
  b.put(position(4, 'h'), piece(piece::silver, piece::rabbit));

  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::gold);

  w = winner(b, piece::silver);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::gold);
}

TEST(winner_test, silver_goal) {
  board b;
  b.put(position(4, 'd'), piece(piece::gold, piece::rabbit));
  b.put(position(1, 'h'), piece(piece::silver, piece::rabbit));

  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::silver);

  w = winner(b, piece::silver);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::silver);
}

TEST(winner_test, attacker_out_of_rabbits) {
  board b;
  b.put(position(4, 'h'), piece(piece::silver, piece::rabbit));

  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::silver);

  w = winner(b, piece::silver);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::silver);
}

TEST(winner_test, defender_out_of_rabbits) {
  board b;
  b.put(position(4, 'h'), piece(piece::gold, piece::rabbit));

  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::gold);

  w = winner(b, piece::silver);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::gold);
}

TEST(winner_test, no_goal) {
  board b;
  b.put(position(7, 'c'), piece(piece::gold, piece::rabbit));
  b.put(position(4, 'd'), piece(piece::silver, piece::rabbit));

  EXPECT_FALSE(winner(b, piece::gold));
  EXPECT_FALSE(winner(b, piece::silver));
}

TEST(winner_test, correct_goal_row_gold) {
  board b;
  b.put(position(3, 'd'), piece(piece::silver, piece::rabbit));
  b.put(position(1, 'c'), piece(piece::gold, piece::rabbit));

  EXPECT_FALSE(winner(b, piece::gold));
  EXPECT_FALSE(winner(b, piece::silver));
}

TEST(winner_test, correct_goal_row_silver) {
  board b;
  b.put(position(8, 'h'), piece(piece::silver, piece::rabbit));
  b.put(position(2, 'b'), piece(piece::gold, piece::rabbit));

  EXPECT_FALSE(winner(b, piece::silver));
  EXPECT_FALSE(winner(b, piece::gold));
}

TEST(winner_test, goal_precedence) {
  board b;
  b.put(position(8, 'c'), piece(piece::gold, piece::rabbit));
  b.put(position(1, 'd'), piece(piece::silver, piece::rabbit));

  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::gold);

  w = winner(b, piece::silver);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::silver);
}

TEST(winner_test, elimination_precedence) {
  board b;
  
  boost::optional<piece::color_t> w = winner(b, piece::gold);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::gold);

  w = winner(b, piece::silver);
  ASSERT_TRUE(w);
  EXPECT_EQ(*w, piece::silver);
}

TEST(search_stack, push_pop_test) {
  board b;
  b.put(position(1, 'a'), piece(piece::gold, piece::dog));
  b.put(position(8, 'h'), piece(piece::silver, piece::cat));

  zobrist_hasher hasher;
  zobrist_hasher::hash_t root_hash =
    hasher.generate_initial(b, piece::gold, MAX_STEPS);
  vertex root;
  root.steps_remaining = MAX_STEPS;
  root.type = vertex::type_or;

  search_stack stack(hasher, root_hash, &root, piece::gold, b);

  EXPECT_TRUE(stack.at_root());

  ASSERT_EQ(1, stack.path().size());
  ASSERT_EQ(1, stack.hashes().size());
  ASSERT_EQ(1, stack.history().size());

  EXPECT_EQ(&root, stack.path().back());
  EXPECT_EQ(root_hash, stack.hashes().back());
  EXPECT_EQ(root_hash, stack.history().back());

  EXPECT_EQ(b, stack.state());

  root.resize(1);
  vertex* child = &*root.begin();
  child->step = step::validate_ordinary_step(b, elementary_step::displacement(position(1, 'a'), north));
  child->type = vertex::type_or;
  child->steps_remaining = MAX_STEPS - 1;

  stack.push(child);

  EXPECT_EQ(2, stack.path().size());
  EXPECT_EQ(2, stack.hashes().size());
  EXPECT_EQ(1, stack.history().size());

  EXPECT_EQ(child, stack.path().back());

  board child_board(b);
  apply(*child->step, child_board);
  zobrist_hasher::hash_t child_hash =
    hasher.generate_initial(child_board, piece::gold, MAX_STEPS - 1);

  EXPECT_FALSE(stack.at_root());
  EXPECT_EQ(child_hash, stack.hashes().back());
  EXPECT_EQ(root_hash, stack.history().back());
  EXPECT_EQ(child_board, stack.state());

  child->resize(1);
  vertex* second_child = &*child->begin();
  second_child->step = step::validate_ordinary_step(child_board, elementary_step::displacement(position(8, 'h'), south));
  second_child->type = vertex::type_and;
  second_child->steps_remaining = MAX_STEPS;

  board second_child_board(child_board);
  apply(*second_child->step, second_child_board);

  zobrist_hasher::hash_t second_child_hash =
    hasher.generate_initial(second_child_board, piece::silver, MAX_STEPS);

  stack.push(second_child);

  EXPECT_EQ(3, stack.path().size());
  EXPECT_EQ(3, stack.hashes().size());
  EXPECT_EQ(2, stack.history().size());

  EXPECT_FALSE(stack.at_root());
  EXPECT_EQ(second_child, stack.path().back());
  EXPECT_EQ(second_child_hash, stack.hashes().back());
  EXPECT_EQ(second_child_hash, stack.history().back());
  EXPECT_EQ(second_child_board, stack.state());

  stack.pop();

  EXPECT_FALSE(stack.at_root());
  EXPECT_EQ(child_hash, stack.hashes().back());
  EXPECT_EQ(root_hash, stack.history().back());
  EXPECT_EQ(child_board, stack.state());

  stack.pop();

  EXPECT_TRUE(stack.at_root());
  EXPECT_EQ(&root, stack.path().back());
  EXPECT_EQ(root_hash, stack.hashes().back());
  EXPECT_EQ(root_hash, stack.history().back());
}

TEST(search_stack, checkpoint_test) {
  board b;
  b.put(position(1, 'a'), piece(piece::gold, piece::dog));
  b.put(position(8, 'h'), piece(piece::silver, piece::cat));

  zobrist_hasher hasher;
  zobrist_hasher::hash_t root_hash =
    hasher.generate_initial(b, piece::gold, MAX_STEPS);
  vertex root;
  root.steps_remaining = MAX_STEPS;

  search_stack stack(hasher, root_hash, &root, piece::gold, b);

  root.resize(1);
  vertex* child = &*root.begin();

  child->step = step::validate_ordinary_step(
    b, elementary_step::displacement(position(1, 'a'), north)
  );
  child->type = vertex::type_or;
  child->steps_remaining = MAX_STEPS - 1;

  stack.push(child);

  board child_board(b);
  apply(*child->step, child_board);

  {
    search_stack_checkpoint checkpoint(stack);
    child->resize(1);

    vertex* second_child = &*child->begin();
    second_child->step = step::validate_ordinary_step(child_board, elementary_step::displacement(position(2, 'a'), north));
    second_child->type = vertex::type_and;
    second_child->steps_remaining = MAX_STEPS - 2;

    stack.push(second_child);
  }

  EXPECT_EQ(2, stack.path().size());
  EXPECT_EQ(child, stack.path().back());
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

