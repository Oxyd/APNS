#include "board.hpp"

#include <gtest/gtest.h>

#include <boost/bind.hpp>

#include <stdexcept>
#include <map>
#include <vector>
#include <iostream>

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

class board_test : public testing::Test {
protected:
  board b;
};

TEST_F(board_test, put_get_remove_test) {
  for (
    position::row_t row = position::MIN_ROW; row <= position::MAX_ROW; ++row
  )
    for (
      position::col_t column = position::MIN_COLUMN;
      column <= position::MAX_COLUMN;
      ++column
    )
      EXPECT_TRUE(!b.get(position(row, column)));

  b.put(position(3, 'g'), piece(piece::gold, piece::horse));

  for (
    position::row_t row = position::MIN_ROW;
    row <= position::MAX_ROW;
    ++row
  )
    for (
      position::col_t column = position::MIN_COLUMN;
      column <= position::MAX_COLUMN;
      ++column
    )
      if (position(row, column) != position(3, 'g'))
        EXPECT_TRUE(!b.get(position(row, column)));
      else
        ASSERT_TRUE(b.get(position(row, column)));

  piece p = *b.get(position(3, 'g'));
  EXPECT_EQ(p.color(), piece::gold);
  EXPECT_EQ(p.type(), piece::horse);

  for (int t = piece::elephant; t <= piece::rabbit; ++t) {
    if (t != piece::horse) {
      EXPECT_TRUE(p.type() != t);
    }
  }
}

TEST_F(board_test, put_remove_invalid_test) {
  position pos(3, 'g');

  b.put(pos, piece(piece::silver, piece::horse));

  EXPECT_THROW(b.put(pos, piece(piece::gold, piece::cat)), std::logic_error);

  EXPECT_NO_THROW(b.remove(position(3, 'g')));
  EXPECT_TRUE(!b.get(position(3, 'g')));

  EXPECT_THROW(b.remove(position(3, 'g')), std::logic_error);
}

TEST_F(board_test, iteration) {
  bool iterated = false;
  for (board::iterator p = b.begin(); p != b.end(); ++p) {
    iterated = true;
  }

  EXPECT_EQ(iterated, false);

  b.put(position(3, 'a'), piece(piece::silver, piece::horse));
  b.put(position(4, 'c'), piece(piece::gold, piece::rabbit));
  b.put(position(1, 'a'), piece(piece::gold, piece::elephant));
  b.put(position(2, 'f'), piece(piece::gold, piece::dog));

  unsigned gold_count = 0;
  unsigned silver_count = 0;

  for (board::iterator piece = b.begin(); piece != b.end(); ++piece) {
    if (piece->second.color() == piece::silver) {
      ++silver_count;
    } else {
      ++gold_count;
    }

    switch (piece->second.type()) {
    case piece::horse:
    case piece::rabbit:
    case piece::elephant:
    case piece::dog:
      continue;  // Okay
    default:
      FAIL() << "Invalid piece type";
    }
  }

  EXPECT_EQ(3, gold_count);
  EXPECT_EQ(1, silver_count);
}

TEST_F(board_test, is_trap_test) {
  EXPECT_TRUE(trap(position(3, 'c')));
  EXPECT_TRUE(trap(position(6, 'c')));
  EXPECT_TRUE(trap(position(3, 'f')));
  EXPECT_TRUE(trap(position(6, 'f')));

  for (
    position::row_t row = position::MIN_ROW;
    row <= position::MAX_ROW;
    ++row
  )
    for (
      position::col_t column = position::MIN_COLUMN;
      column <= position::MAX_COLUMN;
      ++column
    )
      if (row != 3 && row != 6)
        EXPECT_TRUE(!trap(position(row, column)));
      else if (column != 'c' && column != 'f')
        EXPECT_TRUE(!trap(position(row, column)));
}

TEST_F(board_test, string_serialization_test) {
  std::string const s = string_from_board(b);
  board new_board;
  board_from_string(s, new_board);

  EXPECT_EQ(b, new_board);
}

TEST(neighbourhood_iter, interior_iteration_test) {
  position center(3, 'c');

  std::map<position, bool> seen;
  seen[position(2, 'c')] = false;
  seen[position(4, 'c')] = false;
  seen[position(3, 'b')] = false;
  seen[position(3, 'd')] = false;

  for (neighbourhood_iter iterator = neighbourhood_begin(center); iterator != neighbourhood_end(); ++iterator) {
    seen[*iterator] = true;
  }

  EXPECT_EQ(seen.size(), 4);

  for (std::map<position, bool>::iterator i = seen.begin(); i != seen.end(); ++i) {
    EXPECT_EQ(i->second, true);
  }
}

void test_corner(position center) {
  std::size_t count = 0;
  for (neighbourhood_iter it = neighbourhood_begin(center); it != neighbourhood_end(); ++it) {
    ++count;
  }

  EXPECT_EQ(count, 2);
}

TEST(neighbourhood_iter, corners_test) {
  test_corner(position(1, 'a'));
  test_corner(position(8, 'a'));
  test_corner(position(1, 'h'));
  test_corner(position(8, 'h'));
}

class adjacent_pieces_test : public testing::Test {
protected:
  void SetUp() {
    b.put(position(3, 'd'), piece(piece::gold, piece::cat));
    b.put(position(4, 'd'), piece(piece::silver, piece::elephant));
    b.put(position(5, 'd'), piece(piece::gold, piece::camel));
    b.put(position(4, 'e'), piece(piece::silver, piece::cat));
    b.put(position(5, 'e'), piece(piece::silver, piece::dog));
    b.put(position(6, 'e'), piece(piece::silver, piece::rabbit));
    b.put(position(5, 'f'), piece(piece::gold, piece::horse));
  }

  board b;
};

TEST_F(adjacent_pieces_test, one_adjacent) {
  std::vector<piece> adjacent(adjacent_pieces_begin(b, position(3, 'd')), adjacent_pieces_end());
  ASSERT_EQ(adjacent.size(), 1);
  EXPECT_EQ(adjacent[0].color(), piece::silver);
  EXPECT_EQ(adjacent[0].type(), piece::elephant);
}

bool expect_in_it(std::vector<piece> const& pieces, piece::color_t c, piece::type_t t) {
  return std::find(pieces.begin(), pieces.end(), piece(c, t)) != pieces.end();
}

std::ostream& operator << (std::ostream& stream, piece::color_t color) {
  switch (color) {
  case piece::gold:     return stream << "gold";
  case piece::silver:   return stream << "silver";
  default:              return stream << "!!!!";
  }

  return stream;
}

std::ostream& operator << (std::ostream& stream, piece::type_t type) {
  switch (type) {
  case piece::elephant:     return stream << "elephant";
  case piece::camel:        return stream << "camel";
  case piece::horse:        return stream << "horse";
  case piece::dog:          return stream << "dog";
  case piece::cat:          return stream << "cat";
  case piece::rabbit:       return stream << "rabbit";
  default:                  return stream << "??????";
  }

  return stream;
}

std::ostream& operator << (std::ostream& stream, std::vector<piece> const& pieces) {
  for (std::vector<piece>::const_iterator piece = pieces.begin(); piece != pieces.end(); ++piece) {
    if (piece != pieces.begin()) {
      stream << ", ";
    } else {
      stream << "[";
    }
    stream << "(" << piece->color() << ", " << piece->type() << ")";
  }

  stream << "]";
  return stream;
}

TEST_F(adjacent_pieces_test, two_adjacent) {
  std::vector<piece> adjacent(adjacent_pieces_begin(b, position(4, 'e')), adjacent_pieces_end());
  EXPECT_EQ(adjacent.size(), 2);
  EXPECT_PRED3(expect_in_it, adjacent, piece::silver, piece::dog);
  EXPECT_PRED3(expect_in_it, adjacent, piece::silver, piece::dog);
  EXPECT_PRED3(expect_in_it, adjacent, piece::silver, piece::elephant);
}

TEST_F(adjacent_pieces_test, three_adjacent) {
  std::vector<piece> adjacent(adjacent_pieces_begin(b, position(4, 'd')), adjacent_pieces_end());
  EXPECT_EQ(adjacent.size(), 3);
  EXPECT_PRED3(expect_in_it, adjacent, piece::gold, piece::camel);
  EXPECT_PRED3(expect_in_it, adjacent, piece::silver, piece::cat);
  EXPECT_PRED3(expect_in_it, adjacent, piece::gold, piece::cat);
}

TEST_F(adjacent_pieces_test, four_adjacent) {
  std::vector<piece> adjacent(adjacent_pieces_begin(b, position(5, 'e')), adjacent_pieces_end());
  EXPECT_EQ(adjacent.size(), 4);
  EXPECT_PRED3(expect_in_it, adjacent, piece::silver, piece::rabbit);
  EXPECT_PRED3(expect_in_it, adjacent, piece::gold, piece::horse);
  EXPECT_PRED3(expect_in_it, adjacent, piece::silver, piece::cat);
  EXPECT_PRED3(expect_in_it, adjacent, piece::gold, piece::camel);
}

TEST(mask, set_get) {
  board::mask m;
  m.set(position(2, 'c'), true);
  EXPECT_TRUE(m.get(position(2, 'c')));

  m.set(position(2, 'c'), false);
  EXPECT_FALSE(m.get(position(2, 'c')));

  EXPECT_FALSE(m.get(position(3, 'e')));

  m.set(position(1, 'a'), true);
  EXPECT_TRUE(m.get(position(1, 'a')));

  m.set(position(8, 'h'), true);
  EXPECT_TRUE(m.get(position(8, 'h')));
}

TEST(mask, row_test) {
  for (position::row_t r = position::MIN_ROW; r <= position::MAX_ROW; ++r) {
    board::mask m = board::mask::row(r);

    for (
      position::row_t row = position::MIN_ROW;
      row <= position::MAX_ROW;
      ++row)
      for (
        position::col_t col = position::MIN_COLUMN;
        col <= position::MAX_COLUMN;
        ++col
      )
        if (row == r)
          EXPECT_TRUE(m.get(position(row, col))) << row << ' ' << col;
        else
          EXPECT_FALSE(m.get(position(row, col))) << row << ' ' << col;
  }
}

TEST(mask, column_test) {
  for (
    position::col_t c = position::MIN_COLUMN;
    c <= position::MAX_COLUMN;
    ++c
  ) {
    board::mask m = board::mask::column(c);

    for (
      position::row_t row = position::MIN_ROW;
      row <= position::MAX_ROW;
      ++row
    )
      for (
        position::col_t col = position::MIN_COLUMN;
        col <= position::MAX_COLUMN;
        ++col
      )
        if (col == c)
          EXPECT_TRUE(m.get(position(row, col)));
        else
          EXPECT_FALSE(m.get(position(row, col)));
  }
}

TEST(mask, set_test) {
  board::mask m;
  m.set(position(3, 'c'), true);
  m.set(position(3, 'f'), true);
  m.set(position(6, 'c'), true);
  m.set(position(6, 'f'), true);

  EXPECT_TRUE(board::mask::TRAPS == m);
}

TEST(mask, shift_test) {
  EXPECT_EQ(board::mask::column('d'),
            board::mask::column('e').shift(west));
  EXPECT_EQ(board::mask::row(5),
            board::mask::row(6).shift(south));
  EXPECT_TRUE(board::mask::column('h').shift(east).empty());
}

TEST(mask, iteration) {
  board::mask m;
  m.set(position(1, 'a'), true);
  m.set(position(3, 'g'), true);
  m.set(position(8, 'a'), true);
  m.set(position(8, 'h'), true);

  std::set<position> result;
  for (board::mask::iterator it = m.begin(); it != m.end(); ++it)
    result.insert(*it);

  EXPECT_EQ(4, result.size());
  EXPECT_TRUE(result.find(position(1, 'a')) != result.end());
  EXPECT_TRUE(result.find(position(3, 'g')) != result.end());
  EXPECT_TRUE(result.find(position(8, 'a')) != result.end());
  EXPECT_TRUE(result.find(position(8, 'h')) != result.end());
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
