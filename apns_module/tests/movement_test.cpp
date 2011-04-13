#include "movement.hpp"
#include "board.hpp"

#include <gtest/gtest.h>

#include <utility>

TEST(elementary_step, elementary_step_test) {
  for (position::row_t row = board::MIN_ROW; row < board::MAX_ROW; ++row) {
    for (position::col_t column = board::MIN_COLUMN; column < board::MAX_COLUMN; ++column) {
      position pos(row, column);
      direction dir = north;
      elementary_step es = elementary_step::displacement(pos, dir);

      EXPECT_TRUE(es.get_from() == pos);
      EXPECT_EQ(es.get_where(), dir);
      EXPECT_EQ(es.is_capture(), false);

      elementary_step capture = elementary_step::capture(pos);
      EXPECT_TRUE(capture.get_from() == pos);
      EXPECT_EQ(capture.is_capture(), true);
    }
  }
}

TEST(movement, is_frozen_test) {
  board b;

  position silver_dog_pos(4, 'd');
  piece silver_dog(piece::silver, piece::dog);

  b.put(silver_dog_pos, silver_dog);

  EXPECT_TRUE(!frozen(silver_dog_pos, b));

  b.put(position(4, 'e'), piece(piece::gold, piece::elephant));
  EXPECT_TRUE(frozen(silver_dog_pos, b));

  b.put(position(3, 'd'), piece(piece::silver, piece::rabbit));
  EXPECT_TRUE(!frozen(silver_dog_pos, b));

  b.remove(position(4, 'e'));
  b.remove(position(3, 'd'));

  b.put(position(4, 'e'), piece(piece::gold, piece::dog));
  EXPECT_TRUE(!frozen(silver_dog_pos, b));

  b.remove(position(4, 'e'));
  b.put(position(4, 'e'), piece(piece::gold, piece::rabbit));
  EXPECT_TRUE(!frozen(silver_dog_pos, b));

  b.put(position(8, 'a'), piece(piece::gold, piece::elephant));
  EXPECT_TRUE(!frozen(position(8, 'a'), b));
}

TEST(movement, simple_move_test) {
  for (position::row_t row = board::MIN_ROW; row <= board::MAX_ROW; ++row) {
    for (position::col_t column = board::MIN_COLUMN; column <= board::MIN_COLUMN; ++column) {
      position p(row, column);
      board b;
      b.put(p, piece(piece::silver, piece::cat));

      for (direction dir = north; dir <= west; ++(int&)dir) {
        elementary_step el_step = elementary_step::displacement(p, dir);

        if ((dir == north && row == board::MAX_ROW)
            || (dir == east && column == board::MAX_COLUMN)
            || (dir == south && row == board::MIN_ROW)
            || (dir == west && column == board::MIN_COLUMN))
        {
          EXPECT_TRUE(!step::validate_ordinary_step(b, el_step));
        } else {
          EXPECT_TRUE(step::validate_ordinary_step(b, el_step));
        }
      }
    }
  }
}

TEST(movement, simple_move_capture_test) {
  using std::make_pair;

  std::pair<position, direction> const moves[] = {
    make_pair(position(2, 'c'), north),
    make_pair(position(3, 'b'), east),
    make_pair(position(4, 'c'), south),
    make_pair(position(3, 'd'), west),

    make_pair(position(2, 'f'), north),
    make_pair(position(3, 'e'), east),
    make_pair(position(4, 'f'), south),
    make_pair(position(3, 'g'), west),

    make_pair(position(5, 'c'), north),
    make_pair(position(6, 'b'), east),
    make_pair(position(7, 'c'), south),
    make_pair(position(6, 'd'), west),

    make_pair(position(5, 'f'), north),
    make_pair(position(6, 'e'), east),
    make_pair(position(7, 'f'), south),
    make_pair(position(6, 'g'), west)
  };

  std::size_t const moves_count = sizeof(moves) / sizeof(*moves);

  for (piece::type_t type = piece::elephant; type <= piece::cat; ++(int&)type) {
    for (piece::color_t color = piece::gold; color <= piece::silver; ++(int&)color) {
      for (std::pair<position, direction> const* move = moves; move != moves + moves_count; ++move) {
        position const p = move->first;
        direction const d = move->second;

        board b;
        b.put(p, piece(color, type));

        boost::optional<step> s = step::validate_ordinary_step(b, elementary_step::displacement(p, d));
        ASSERT_TRUE(s);
        EXPECT_TRUE(s->capture());
      }
    }
  }
}

TEST(movement, simple_move_capture_test_2) {
  {
    board b;
    b.put(position(4, 'c'), piece(piece::gold, piece::elephant));
    b.put(position(3, 'b'), piece(piece::gold, piece::horse));

    boost::optional<step> s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'b'), east));
    ASSERT_TRUE(s);
    EXPECT_TRUE(!s->capture());
  }
  {
    board b;
    b.put(position(3, 'c'), piece(piece::silver, piece::camel));
    b.put(position(3, 'd'), piece(piece::silver, piece::cat));

    boost::optional<step> s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'd'), east));
    ASSERT_TRUE(s);
    EXPECT_TRUE(s->capture());

    s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'd'), west));
    EXPECT_TRUE(!s);
  }
}

TEST(movement, simple_push_move_test) {
  board b;

  b.put(position(2, 'b'), piece(piece::gold, piece::camel));
  b.put(position(2, 'c'), piece(piece::silver, piece::dog));

  EXPECT_TRUE(step::validate_push(b, elementary_step::displacement(position(2, 'c'), east),
                                     elementary_step::displacement(position(2, 'b'), east)));
  b.put(position(1, 'c'), piece(piece::gold, piece::elephant));
  EXPECT_TRUE(step::validate_push(b, elementary_step::displacement(position(2, 'c'), east),
                                     elementary_step::displacement(position(2, 'b'), east)));

  b.put(position(1, 'b'), piece(piece::silver, piece::elephant));
  EXPECT_TRUE(!step::validate_push(b, elementary_step::displacement(position(2, 'c'), east),
                                      elementary_step::displacement(position(2, 'b'), east)));

  b.remove(position(1, 'c'));
  b.remove(position(1, 'b'));

  EXPECT_TRUE(!step::validate_push(b, elementary_step::displacement(position(3, 'f'), west),
                                      elementary_step::displacement(position(4, 'a'), north)));

  EXPECT_TRUE(!step::validate_push(b, elementary_step::displacement(position(2, 'c'), west),
                                      elementary_step::displacement(position(2, 'b'), east)));

  b.put(position(2, 'd'), piece(piece::gold, piece::rabbit));
  EXPECT_TRUE(!step::validate_push(b, elementary_step::displacement(position(2, 'c'), east),
                                      elementary_step::displacement(position(2, 'b'), east)));
}

TEST(movement, simple_push_capture_test) {
  board b;

  b.put(position(3, 'b'), piece(piece::gold, piece::cat));
  b.put(position(2, 'b'), piece(piece::silver, piece::horse));

  boost::optional<step> s = step::validate_push(b,
      elementary_step::displacement(position(3, 'b'), east),
      elementary_step::displacement(position(2, 'b'), north));
  ASSERT_TRUE(s);
  EXPECT_TRUE(s->capture());

  b.put(position(3, 'e'), piece(piece::gold, piece::elephant));
  b.put(position(3, 'f'), piece(piece::silver, piece::dog));
  b.put(position(3, 'g'), piece(piece::silver, piece::rabbit));

  s = step::validate_push(b,
      elementary_step::displacement(position(3, 'f'), north),
      elementary_step::displacement(position(3, 'e'), east));

  ASSERT_TRUE(s);
  EXPECT_TRUE(s->capture());

  b.put(position(5, 'e'), piece(piece::silver, piece::camel));
  b.put(position(6, 'e'), piece(piece::gold, piece::dog));
  b.put(position(6, 'f'), piece(piece::gold, piece::camel));

  s = step::validate_push(b,
      elementary_step::displacement(position(6, 'e'), north),
      elementary_step::displacement(position(5, 'e'), north));

  ASSERT_TRUE(s);
  EXPECT_TRUE(s->capture());
}

TEST(movement, simple_pull_move_test) {
  board b;

  b.put(position(2, 'b'), piece(piece::silver, piece::dog));
  b.put(position(2, 'c'), piece(piece::gold, piece::camel));

  EXPECT_TRUE(step::validate_pull(b, elementary_step::displacement(position(2, 'c'), east),
                                     elementary_step::displacement(position(2, 'b'), east)));

  b.put(position(1, 'b'), piece(piece::gold, piece::elephant));
  EXPECT_TRUE(step::validate_pull(b, elementary_step::displacement(position(2, 'c'), east),
                                      elementary_step::displacement(position(2, 'b'), east)));

  b.put(position(1, 'c'), piece(piece::silver, piece::elephant));
  EXPECT_TRUE(!step::validate_pull(b, elementary_step::displacement(position(2, 'c'), east),
                                     elementary_step::displacement(position(2, 'b'), east)));
  b.remove(position(1, 'c'));
  b.remove(position(1, 'b'));

  EXPECT_TRUE(!step::validate_pull(b, elementary_step::displacement(position(3, 'f'), west),
                                      elementary_step::displacement(position(4, 'a'), north)));

  EXPECT_TRUE(!step::validate_pull(b, elementary_step::displacement(position(2, 'c'), west),
                                      elementary_step::displacement(position(2, 'b'), east)));

  b.put(position(2, 'd'), piece(piece::gold, piece::rabbit));
  EXPECT_TRUE(!step::validate_pull(b, elementary_step::displacement(position(2, 'c'), east),
                                      elementary_step::displacement(position(2, 'b'), east)));
}

TEST(movement, simple_pull_capture_test) {
  board b;

  b.put(position(2, 'b'), piece(piece::gold, piece::cat));
  b.put(position(3, 'b'), piece(piece::silver, piece::horse));

  boost::optional<step> s = step::validate_pull(b,
      elementary_step::displacement(position(3, 'b'), north),
      elementary_step::displacement(position(2, 'b'), north));
  ASSERT_TRUE(s);
  EXPECT_TRUE(!s->capture());

  b.put(position(3, 'f'), piece(piece::gold, piece::elephant));
  b.put(position(3, 'g'), piece(piece::gold, piece::rabbit));
  b.put(position(2, 'f'), piece(piece::silver, piece::camel));

  s = step::validate_pull(b,
      elementary_step::displacement(position(3, 'f'), north),
      elementary_step::displacement(position(2, 'f'), north));
  ASSERT_TRUE(s);
  EXPECT_TRUE(s->capture());

  b.put(position(6, 'f'), piece(piece::gold, piece::cat));
  b.put(position(6, 'g'), piece(piece::gold, piece::dog));
  b.put(position(7, 'g'), piece(piece::silver, piece::horse));

  s = step::validate_pull(b,
      elementary_step::displacement(position(7, 'g'), north),
      elementary_step::displacement(position(6, 'g'), north));
  ASSERT_TRUE(s);
  EXPECT_TRUE(s->capture());
}

TEST(movement, sacrifice_test) {
  board b;
  b.put(position(6, 'f'), piece(piece::gold, piece::elephant));
  b.put(position(7, 'f'), piece(piece::gold, piece::rabbit));

  boost::optional<step> s = step::validate_ordinary_step(b, elementary_step::displacement(position(7, 'f'), north));
  ASSERT_TRUE(s);
  EXPECT_TRUE(s->capture());

  step::elementary_step_seq el_steps(s->step_sequence_begin(), s->step_sequence_end());
  ASSERT_EQ(2, el_steps.size());

  elementary_step const& e1 = el_steps.front();
  elementary_step const& e2 = el_steps.back();

  ASSERT_TRUE(e1.get_what());
  EXPECT_EQ(piece::gold, e1.get_what()->get_color());
  EXPECT_EQ(piece::rabbit, e1.get_what()->get_type());

  ASSERT_TRUE(e2.get_what());
  EXPECT_EQ(piece::gold, e2.get_what()->get_color());
  EXPECT_EQ(piece::elephant, e2.get_what()->get_type());
}

class move_generation : public testing::Test {
protected:
  void SetUp() {
    b.put(position(4, 'd'), piece(piece::gold, piece::dog));

    b.put(position(7, 'g'), piece(piece::gold, piece::horse));
    b.put(position(8, 'g'), piece(piece::silver, piece::rabbit));

    b.put(position(5, 'b'), piece(piece::gold, piece::elephant));
    b.put(position(5, 'a'), piece(piece::silver, piece::dog));
    b.put(position(5, 'c'), piece(piece::silver, piece::cat));
    b.put(position(6, 'b'), piece(piece::silver, piece::cat));

    b.put(position(2, 'b'), piece(piece::gold, piece::rabbit));

    b.put(position(1, 'h'), piece(piece::gold, piece::cat));
    b.put(position(1, 'g'), piece(piece::silver, piece::dog));
    b.put(position(2, 'g'), piece(piece::silver, piece::horse));
    b.put(position(2, 'h'), piece(piece::silver, piece::horse));
  }

  board b;
};

void expect_counts(board const& b, std::vector<step> const& steps,
    std::size_t expected_total, std::size_t expected_ordinary,
    std::size_t expected_push, std::size_t expected_pull) {
  EXPECT_EQ(steps.size(), expected_total);

  std::size_t ordinary_count = 0;
  std::size_t pull_count = 0;
  std::size_t push_count = 0;

  for (std::vector<step>::const_iterator s = steps.begin(); s != steps.end(); ++s) {
    switch (step_kind(*s, piece::gold, b)) {
    case ordinary:  ++ordinary_count;   break;
    case push:      ++push_count;       break;
    case pull:      ++pull_count;       break;
    }
  }

  EXPECT_EQ(ordinary_count, expected_ordinary);
  EXPECT_EQ(push_count, expected_push);
  EXPECT_EQ(pull_count, expected_pull);
}

TEST_F(move_generation, ordinary_step_generation) {
  std::vector<step> steps;
  steps.insert(steps.end(), steps_begin(position(4, 'd'), b), steps_end());
  expect_counts(b, steps, 4, 4, 0, 0);
}

TEST_F(move_generation, push_pull_step_generation) {
  std::vector<step> steps;
  steps.insert(steps.end(), steps_begin(position(7, 'g'), b), steps_end());
  expect_counts(b, steps, 8, 3, 2, 3);
}

TEST_F(move_generation, multiple_push_pull_generation) {
  std::vector<step> steps;
  steps.insert(steps.end(), steps_begin(position(5, 'b'), b), steps_end());
  expect_counts(b, steps, 12, 1, 8, 3);
}

TEST_F(move_generation, rabbit_steps_test) {
  std::vector<step> steps;
  steps.insert(steps.end(), steps_begin(position(2, 'b'), b), steps_end());
  expect_counts(b, steps, 3, 3, 0, 0);
}

TEST_F(move_generation, blockade_test) {
  std::vector<step> steps;
  steps.insert(steps.end(), steps_begin(position(1, 'h'), b), steps_end());
  expect_counts(b, steps, 0, 0, 0, 0);
}

TEST_F(move_generation, all_steps) {
  std::vector<step> steps;
  steps.insert(steps.end(), all_steps_begin(b, piece::gold), all_steps_end());
  expect_counts(b, steps, 27, 11, 10, 6);
}

class move_generation2 : public testing::Test {
protected:
  void SetUp() {
    b.put(position(4, 'b'), piece(piece::gold, piece::rabbit));
    b.put(position(7, 'e'), piece(piece::silver, piece::rabbit));
  }

  board b;
};

TEST_F(move_generation2, steps) {
  std::vector<step> steps;
  steps.insert(steps.end(), steps_begin(position(4, 'b'), b), steps_end());
  expect_counts(b, steps, 3, 3, 0, 0);
}

TEST_F(move_generation2, all_steps) {
  std::vector<step> steps;
  steps.insert(steps.end(), all_steps_begin(b, piece::gold), all_steps_end());
  expect_counts(b, steps, 3, 3, 0, 0);
}

class apply_test : public testing::Test {
protected:
  board b;
  board original;

  void SetUp() {
    b.put(position(1, 'c'), piece(piece::gold, piece::cat));

    b.put(position(3, 'c'), piece(piece::gold, piece::horse));
    b.put(position(3, 'd'), piece(piece::silver, piece::dog));
    b.put(position(4, 'c'), piece(piece::gold, piece::rabbit));

    b.put(position(6, 'b'), piece(piece::gold, piece::cat));

    b.put(position(5, 'e'), piece(piece::gold, piece::horse));
    b.put(position(6, 'e'), piece(piece::silver, piece::rabbit));

    b.put(position(3, 'g'), piece(piece::gold, piece::rabbit));
    b.put(position(4, 'g'), piece(piece::silver, piece::horse));

    b.put(position(6, 'g'), piece(piece::silver, piece::elephant));
    b.put(position(7, 'g'), piece(piece::gold, piece::dog));

    original = b;
  }
};

TEST_F(apply_test, ordinary_move) {
  step s = *step::validate_ordinary_step(b, elementary_step::displacement(position(1, 'c'), east));
  apply(s, b);
  EXPECT_FALSE(b.get(position(1, 'c')));
  ASSERT_TRUE(b.get(position(1, 'd')));
  EXPECT_EQ(b.get(position(1, 'd'))->get_type(), piece::cat);
  EXPECT_EQ(b.get(position(1, 'd'))->get_color(), piece::gold);

  unapply(s, b);
  EXPECT_TRUE(b == original);
}

TEST_F(apply_test, ordinary_suicide) {
  step s = *step::validate_ordinary_step(b, elementary_step::displacement(position(6, 'b'), east));
  apply(s, b);
  EXPECT_FALSE(b.get(position(6, 'b')));
  EXPECT_FALSE(b.get(position(6, 'c')));

  unapply(s, b);
  EXPECT_TRUE(b == original);
}

TEST_F(apply_test, pull) {
  step s = *step::validate_pull(b,
      elementary_step::displacement(position(5, 'e'), south),
      elementary_step::displacement(position(6, 'e'), south));
  apply(s, b);
  EXPECT_FALSE(b.get(position(6, 'e')));
  ASSERT_TRUE(b.get(position(5, 'e')));
  EXPECT_EQ(b.get(position(5, 'e'))->get_type(), piece::rabbit);
  ASSERT_TRUE(b.get(position(4, 'e')));
  EXPECT_EQ(b.get(position(4, 'e'))->get_type(), piece::horse);

  unapply(s, b);
  EXPECT_TRUE(b == original);
}

TEST_F(apply_test, push) {
  step s = *step::validate_push(b,
      elementary_step::displacement(position(7, 'g'), west),
      elementary_step::displacement(position(6, 'g'), north));
  apply(s, b);
  EXPECT_FALSE(b.get(position(6, 'g')));
  ASSERT_TRUE(b.get(position(7, 'f')));
  EXPECT_EQ(b.get(position(7, 'f'))->get_type(), piece::dog);
  ASSERT_TRUE(b.get(position(7, 'g')));
  EXPECT_EQ(b.get(position(7, 'g'))->get_type(), piece::elephant);

  unapply(s, b);
  EXPECT_TRUE(b == original);
}

TEST_F(apply_test, push_capture) {
  step s = *step::validate_push(b,
      elementary_step::displacement(position(3, 'g'), east),
      elementary_step::displacement(position(4, 'g'), south));
  apply(s, b);
  EXPECT_FALSE(b.get(position(4, 'g')));
  EXPECT_FALSE(b.get(position(3, 'f')));
  ASSERT_TRUE(b.get(position(3, 'g')));
  EXPECT_EQ(b.get(position(3, 'g'))->get_type(), piece::horse);

  unapply(s, b);
  EXPECT_TRUE(b == original);
}

TEST_F(apply_test, pull_capture) {
  step s = *step::validate_pull(b,
      elementary_step::displacement(position(3, 'c'), west),
      elementary_step::displacement(position(3, 'd'), west));
  apply(s, b);
  EXPECT_FALSE(b.get(position(3, 'c')));
  ASSERT_TRUE(b.get(position(3, 'b')));
  EXPECT_EQ(b.get(position(3, 'b'))->get_type(), piece::horse);

  unapply(s, b);
  EXPECT_TRUE(b == original);
}

class string_test : public testing::Test {
protected:
  board b;

  void SetUp() {
    b.put(position(2, 'b'), piece(piece::silver, piece::cat));
    b.put(position(3, 'g'), piece(piece::gold, piece::rabbit));
    b.put(position(5, 'b'), piece(piece::gold, piece::horse));
    b.put(position(5, 'c'), piece(piece::silver, piece::cat));
    b.put(position(6, 'f'), piece(piece::gold, piece::elephant));
    b.put(position(7, 'f'), piece(piece::gold, piece::rabbit));
  }
};

std::ostream& operator << (std::ostream& output, step const& step) {
  output << step.to_string();
  return output;
}

TEST_F(string_test, displacement) {
  boost::optional<step> s = step::validate_ordinary_step(b, elementary_step::displacement(position(2, 'b'), east));
  ASSERT_TRUE(s);
  EXPECT_EQ("cb2e", s->to_string());
  boost::optional<step> q = step::from_string("cb2e");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, suicide) {
  boost::optional<step> s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'g'), west));
  ASSERT_TRUE(s);
  EXPECT_EQ("Rg3w Rf3x", s->to_string());
  boost::optional<step> q = step::from_string("Rg3w Rf3x");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, push) {
  boost::optional<step> s = step::validate_push(b,
      elementary_step::displacement(position(5, 'c'), south),
      elementary_step::displacement(position(5, 'b'), east));
  ASSERT_TRUE(s);
  EXPECT_EQ("cc5s Hb5e", s->to_string());
  boost::optional<step> q = step::from_string("cc5s Hb5e");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, pull) {
  boost::optional<step> s = step::validate_pull(b,
      elementary_step::displacement(position(5, 'b'), north),
      elementary_step::displacement(position(5, 'c'), west));
  ASSERT_TRUE(s);
  EXPECT_EQ("Hb5n cc5w", s->to_string());
  boost::optional<step> q = step::from_string("Hb5n cc5w");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, capture) {
  boost::optional<step> s = step::validate_push(b,
      elementary_step::displacement(position(5, 'c'), north),
      elementary_step::displacement(position(5, 'b'), east));
  ASSERT_TRUE(s);
  EXPECT_EQ("cc5n cc6x Hb5e", s->to_string());
  boost::optional<step> q = step::from_string("cc5n cc6x Hb5e");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, sacrifice) {
  boost::optional<step> s = step::validate_ordinary_step(b, elementary_step::displacement(position(7, 'f'), north));
  ASSERT_TRUE(s);
  EXPECT_EQ("Rf7n Ef6x", s->to_string());
  boost::optional<step> q = step::from_string("Rf7n Ef6x");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
