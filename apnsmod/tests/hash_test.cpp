#include "hash.hpp"
#include "board.hpp"
#include "movement.hpp"

#include <gtest/gtest.h>

TEST(zobrist_test, construction_test) {
  apns::zobrist_hasher z;
}

TEST(zobrist_test, hash_equality_test) {
  using namespace apns;

  zobrist_hasher hasher;
  board b;
  b.put(position(2, 'c'), piece(piece::silver, piece::camel));
  zobrist_hasher::hash_t h = hasher.generate_initial(b, piece::silver,
                                                     MAX_STEPS);

  board c;
  c.put(position(2, 'c'), piece(piece::silver, piece::camel));

  EXPECT_EQ(h, hasher.generate_initial(c, piece::silver, MAX_STEPS));
  EXPECT_NE(h, hasher.generate_initial(c, piece::gold, MAX_STEPS));
}

TEST(zobrist_test, simple_move_test) {
  using namespace apns;

  zobrist_hasher hasher;
  board initial;
  initial.put(position(2, 'c'), piece(piece::gold, piece::dog));

  boost::optional<step> s = step::validate_ordinary_step(
    initial, elementary_step::displacement(position(2, 'c'), east)
  );
  ASSERT_TRUE(s);

  zobrist_hasher::hash_t initial_hash =
    hasher.generate_initial(initial, piece::gold, MAX_STEPS);
  zobrist_hasher::hash_t terminal_hash = hasher.update(
    initial_hash, s->begin(), s->end(),
    piece::gold, piece::gold, MAX_STEPS
  );

  board terminal;
  terminal.put(position(2, 'd'), piece(piece::gold, piece::dog));

  EXPECT_EQ(
    hasher.generate_initial(
      terminal, piece::gold, MAX_STEPS - 1),
    terminal_hash
  );
}

TEST(zobrist_test, pull_move_test) {
  using namespace apns;

  zobrist_hasher hasher;
  board initial;
  initial.put(position(3, 'e'), piece(piece::gold, piece::elephant));
  initial.put(position(2, 'e'), piece(piece::silver, piece::cat));

  boost::optional<step> s = step::validate_pull(initial,
      elementary_step::displacement(position(3, 'e'), north),
      elementary_step::displacement(position(2, 'e'), north));
  ASSERT_TRUE(s);

  board terminal = initial;
  apply(*s, terminal);

  zobrist_hasher::hash_t initial_h =
    hasher.generate_initial(initial, piece::gold, MAX_STEPS);
  zobrist_hasher::hash_t terminal_h =
    hasher.generate_initial(terminal, piece::gold, MAX_STEPS - 2);
  zobrist_hasher::hash_t resulting_h =
    hasher.update(
    initial_h,
    s->begin(), s->end(),
    piece::gold, piece::gold,
    MAX_STEPS
  );

  EXPECT_EQ(terminal_h, resulting_h);
}

TEST(zobrist_test, end_turn_test) {
  using namespace apns;

  zobrist_hasher hasher;
  board initial;
  initial.put(position(3, 'e'), piece(piece::gold, piece::elephant));
  initial.put(position(2, 'e'), piece(piece::silver, piece::cat));

  boost::optional<step> s = step::validate_ordinary_step(
    initial,
    elementary_step::displacement(position(3, 'e'), north)
  );
  ASSERT_TRUE(s);

  board terminal = initial;
  apply(*s, terminal);

  zobrist_hasher::hash_t initial_h =
    hasher.generate_initial(initial, piece::gold, MAX_STEPS);
  zobrist_hasher::hash_t terminal_h =
    hasher.generate_initial(terminal, piece::silver, MAX_STEPS);
  zobrist_hasher::hash_t resulting_h = hasher.update(
    initial_h,
    s->begin(), s->end(),
    piece::gold, piece::silver, MAX_STEPS
  );

  EXPECT_EQ(terminal_h, resulting_h);
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

