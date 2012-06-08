#include "position.hpp"
#include "board.hpp"

#include <gtest/gtest.h>

using namespace apns;

class position_test : public testing::Test {
};

TEST_F(position_test, construct) {
  for (int row = 1; row <= 8; ++row) {
    for (char column = 'a'; column <= 'h'; ++column) {
      position temp(row, column);
    }
  }
}

TEST_F(position_test, range_test) {
  EXPECT_THROW(position p2(1, 'x'), std::logic_error);
  EXPECT_THROW(position p3(1, 'B'), std::logic_error);
  EXPECT_THROW(position p4(1, '?'), std::logic_error);
  EXPECT_THROW(position p5(12, 'c'), std::logic_error);
  EXPECT_THROW(position p6(9, 'd'), std::logic_error);
  EXPECT_THROW(position p7(0, 'e'), std::logic_error);
  EXPECT_THROW(position p8(-1, 'f'), std::logic_error);
  EXPECT_THROW(position p9(13, 'z'), std::logic_error);
}

TEST_F(position_test, comparison) {
  EXPECT_TRUE(position(1, 'a') == position(1, 'a'));
  EXPECT_TRUE(position(1, 'b') != position(3, 'c'));
  EXPECT_TRUE(position(1, 'b') != position(1, 'a'));
  EXPECT_TRUE(position(1, 'b') != position(3, 'b'));
}

TEST_F(position_test, linear_order) {
  position p1(1, 'a');
  position p2(1, 'b');
  position p3(2, 'a');
  position p4(2, 'c');

  // Test that they all compare inequal.
  EXPECT_TRUE(p1 < p2 || p2 < p1);
  EXPECT_TRUE(p1 < p3 || p3 < p1);
  EXPECT_TRUE(p1 < p4 || p4 < p1);
  EXPECT_TRUE(p2 < p3 || p3 < p2);
  EXPECT_TRUE(p2 < p4 || p4 < p2);
  EXPECT_TRUE(p3 < p4 || p4 < p3);

  position p5 = p1;
  EXPECT_TRUE(!(p1 < p5) && !(p5 < p1));
}

TEST_F(position_test, increment_test) {
  position p1(position::MIN_ROW, position::MIN_COLUMN);
  position p2(position::MIN_ROW, position::MIN_COLUMN);

  for (std::size_t i = 0; i < board::ROWS * board::COLUMNS; ++i) {
    position p3 = p1 + i;

    position::row_t r = i / board::ROWS + position::MIN_ROW;
    position::col_t c = i % board::COLUMNS + position::MIN_COLUMN;

    EXPECT_EQ(r, p3.row());
    EXPECT_EQ(c, p3.column());

    EXPECT_EQ(r, p2.row());
    EXPECT_EQ(c, p2.column());

    ++p2;
  }

  EXPECT_THROW(++p2, std::logic_error);
  EXPECT_THROW(p1 + 65, std::logic_error);
  EXPECT_THROW(p1 - 1, std::logic_error);
  EXPECT_THROW(--p1, std::logic_error);
}

TEST_F(position_test, make_adjacent_test) {
  position pos(1, 'a');

  pos = make_adjacent(pos, east);
  EXPECT_EQ(pos.row(), 1);
  EXPECT_EQ(pos.column(), 'b');

  pos = make_adjacent(pos, north);
  EXPECT_EQ(pos.row(), 2);
  EXPECT_EQ(pos.column(), 'b');

  pos = make_adjacent(pos, west);
  EXPECT_EQ(pos.row(), 2);
  EXPECT_EQ(pos.column(), 'a');

  pos = make_adjacent(pos, south);
  EXPECT_EQ(pos.row(), 1);
  EXPECT_EQ(pos.column(), 'a');

  EXPECT_THROW(make_adjacent(pos, south), std::logic_error);
  EXPECT_THROW(make_adjacent(pos, west), std::logic_error);

  pos = position(8, 'h');
  EXPECT_THROW(make_adjacent(pos, north), std::logic_error);
  EXPECT_THROW(make_adjacent(pos, east), std::logic_error);
}

TEST_F(position_test, is_adjacent_valid_test) {
  position pos(1, 'a');

  EXPECT_EQ(adjacent_valid(pos, east), true);
  EXPECT_EQ(adjacent_valid(pos, north), true);
  EXPECT_EQ(adjacent_valid(pos, west), false);
  EXPECT_EQ(adjacent_valid(pos, south), false);

  pos = position(8, 'a');

  EXPECT_EQ(adjacent_valid(pos, east), true);
  EXPECT_EQ(adjacent_valid(pos, north), false);
  EXPECT_EQ(adjacent_valid(pos, west), false);
  EXPECT_EQ(adjacent_valid(pos, south), true);

  pos = position(8, 'h');

  EXPECT_EQ(adjacent_valid(pos, east), false);
  EXPECT_EQ(adjacent_valid(pos, north), false);
  EXPECT_EQ(adjacent_valid(pos, west), true);
  EXPECT_EQ(adjacent_valid(pos, south), true);

  pos = position(3, 'c');

  EXPECT_EQ(adjacent_valid(pos, east), true);
  EXPECT_EQ(adjacent_valid(pos, north), true);
  EXPECT_EQ(adjacent_valid(pos, west), true);
  EXPECT_EQ(adjacent_valid(pos, south), true);
}

TEST_F(position_test, order_test) {
  position p1(1, 'a');
  position p2(1, 'a');

  for (std::size_t i = 0; i < board::ROWS * board::COLUMNS; ++i) {
    position p3 = p1 + i;
    EXPECT_EQ(i, p3.order());
    EXPECT_EQ(i, p2.order());
    ++p2;

    position::row_t row = i / board::COLUMNS + position::MIN_ROW;
    position::col_t col = i % board::COLUMNS + position::MIN_COLUMN;
    EXPECT_EQ(i, position(row, col).order());
  }
}

TEST_F(position_test, adjacent_test) {
  EXPECT_EQ(adjacent(position(3, 'c'), position(2, 'c')), true);
  EXPECT_EQ(adjacent(position(3, 'c'), position(4, 'c')), true);
  EXPECT_EQ(adjacent(position(3, 'c'), position(3, 'b')), true);
  EXPECT_EQ(adjacent(position(3, 'c'), position(3, 'd')), true);

  EXPECT_EQ(adjacent(position(3, 'c'), position(3, 'e')), false);
  EXPECT_EQ(adjacent(position(3, 'c'), position(5, 'f')), false);

  EXPECT_EQ(adjacent(position(3, 'c'), position(4, 'd')), false);
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
