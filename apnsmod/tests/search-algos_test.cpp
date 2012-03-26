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
  vertex::children_iterator found = traverse(g->root, &best_successor);

  ASSERT_TRUE(found);
  EXPECT_EQ(1, found->proof_number);
  EXPECT_EQ(7, found->disproof_number);
  EXPECT_EQ(4, found->steps_remaining);
  EXPECT_EQ(vertex::type_and, found->type);
}

TEST(traversing, hash_test) {
  board target;
  boost::shared_ptr<game> g = make_game(target);

  zobrist_hasher hasher;
  zobrist_hasher::hash_t initial_hash = hasher.generate_initial(g->initial_state, g->attacker);

  hash_visitor hash_v(hasher, initial_hash, g->attacker);
  traverse(g->root, &best_successor, boost::ref(hash_v));

  EXPECT_EQ(hasher.generate_initial(target, piece::silver), hash_v.hashes().back());
}

TEST(traversing, board_test) {
  board target;
  boost::shared_ptr<game> g = make_game(target);

  board_visitor<> board_v(g->initial_state);
  traverse(g->root, &best_successor, boost::ref(board_v));

  EXPECT_EQ(target, board_v.get_board());
}

TEST(traversing, history_test) {
  board target;
  boost::shared_ptr<game> g = make_game(target);

  history_visitor history_v(g->attacker);
  board_visitor<boost::reference_wrapper<history_visitor> > board_v(g->initial_state, boost::ref(history_v));
  traverse(g->root, &best_successor, boost::ref(board_v));

  history_stack::records_cont const& h = history_v.get_history();

  ASSERT_EQ(2, h.size());
  EXPECT_EQ(g->initial_state, h[0].position);
  EXPECT_EQ(target, h[1].position);

  EXPECT_EQ(piece::gold, h[0].player);
  EXPECT_EQ(piece::silver, h[1].player);
}

TEST(traversing, path_test) {
  boost::shared_ptr<game> g = make_game();

  path_visitor path_v;
  traverse(g->root, &best_successor, boost::ref(path_v));

  EXPECT_EQ(4, path_v.path.size());
  vertex::number_t dn = 3;
  for (path_visitor::path_cont::iterator it = path_v.path.begin(); it != path_v.path.end(); ++it)
    EXPECT_EQ(++dn, (*it)->disproof_number);
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

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

