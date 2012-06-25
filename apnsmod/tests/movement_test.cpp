#include "movement.hpp"
#include "board.hpp"

#include <gtest/gtest.h>

#include <utility>
#include <vector>
#include <set>
#include <string>

using namespace apns;

TEST(elementary_step, elementary_step_test) {
  for (
    position::row_t row = position::MIN_ROW;
    row < position::MAX_ROW;
    ++row
  )
    for (
      position::col_t column = position::MIN_COLUMN;
      column < position::MAX_COLUMN;
      ++column
    ) {
      position pos(row, column);
      direction dir = north;
      elementary_step es = elementary_step::displacement(pos, dir);

      EXPECT_TRUE(es.from() == pos);
      EXPECT_EQ(es.where(), dir);
      EXPECT_EQ(es.capture(), false);

      elementary_step capture = elementary_step::make_capture(pos);
      EXPECT_TRUE(capture.from() == pos);
      EXPECT_EQ(capture.capture(), true);
    }
}

TEST(movement, is_frozen_test) {
  board b;

  position silver_dog_pos(4, 'd');
  piece silver_dog(piece::silver, piece::dog);

  b.put(silver_dog_pos, silver_dog);

  EXPECT_TRUE(!frozen(silver_dog_pos, b));
  EXPECT_TRUE(mobile(silver_dog_pos, b));

  b.put(position(4, 'e'), piece(piece::gold, piece::elephant));
  EXPECT_TRUE(frozen(silver_dog_pos, b));
  EXPECT_FALSE(mobile(silver_dog_pos, b));

  b.put(position(3, 'd'), piece(piece::silver, piece::rabbit));
  EXPECT_TRUE(!frozen(silver_dog_pos, b));
  EXPECT_TRUE(mobile(silver_dog_pos, b));

  b.remove(position(4, 'e'));
  b.remove(position(3, 'd'));

  b.put(position(4, 'e'), piece(piece::gold, piece::dog));
  EXPECT_TRUE(!frozen(silver_dog_pos, b));
  EXPECT_TRUE(mobile(silver_dog_pos, b));

  b.remove(position(4, 'e'));
  b.put(position(4, 'e'), piece(piece::gold, piece::rabbit));
  EXPECT_TRUE(!frozen(silver_dog_pos, b));
  EXPECT_TRUE(mobile(silver_dog_pos, b));

  b.put(position(8, 'a'), piece(piece::gold, piece::elephant));
  EXPECT_TRUE(!frozen(position(8, 'a'), b));
  EXPECT_TRUE(mobile(position(8, 'a'), b));
}

TEST(movement, simple_move_test) {
  for (
    position::row_t row = position::MIN_ROW;
    row <= position::MAX_ROW;
    ++row
  )
    for (
      position::col_t column = position::MIN_COLUMN;
      column <= position::MIN_COLUMN;
      ++column
    ) {
      position p(row, column);
      board b;
      b.put(p, piece(piece::silver, piece::cat));

      for (int d = north; d <= west; ++d) {
        direction dir = static_cast<direction>(d);
        elementary_step el_step = elementary_step::displacement(p, dir);

        if ((dir == north && row == position::MAX_ROW) ||
            (dir == east && column == position::MAX_COLUMN) ||
            (dir == south && row == position::MIN_ROW) ||
            (dir == west && column == position::MIN_COLUMN))
        {
          EXPECT_TRUE(!step::validate_ordinary_step(b, el_step));
        } else {
          EXPECT_TRUE(step::validate_ordinary_step(b, el_step));
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

  for (int t = piece::elephant; t <= piece::cat; ++t) {
    for (int c = piece::gold; c <= piece::silver; ++c) {
      piece::type_t type = static_cast<piece::type_t>(t);
      piece::color_t color = static_cast<piece::color_t>(c);

      for (std::pair<position, direction> const* move = moves; move != moves + moves_count; ++move) {
        position const p = move->first;
        direction const d = move->second;

        board b;
        b.put(p, piece(color, type));

        step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(p, d));
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

    step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'b'), east));
    ASSERT_TRUE(s);
    EXPECT_TRUE(!s->capture());
  }
  {
    board b;
    b.put(position(3, 'c'), piece(piece::silver, piece::camel));
    b.put(position(3, 'd'), piece(piece::silver, piece::cat));

    step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'd'), east));
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

  step_holder s = step::validate_push(b,
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

  step_holder s = step::validate_pull(b,
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

  step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(position(7, 'f'), north));
  ASSERT_TRUE(s);
  EXPECT_TRUE(s->capture());

  std::vector<elementary_step> el_steps(s->begin(), s->end());
  ASSERT_EQ(2, el_steps.size());

  elementary_step const& e1 = el_steps.front();
  elementary_step const& e2 = el_steps.back();

  ASSERT_TRUE(e1.what());
  EXPECT_EQ(piece::gold, e1.what()->color());
  EXPECT_EQ(piece::rabbit, e1.what()->type());

  ASSERT_TRUE(e2.what());
  EXPECT_EQ(piece::gold, e2.what()->color());
  EXPECT_EQ(piece::elephant, e2.what()->type());
}

TEST(movement, simple_move_loss_test) {
  board b;
  b.put(position(3, 'b'), piece(piece::gold, piece::elephant));
  b.put(position(3, 'c'), piece(piece::gold, piece::horse));
  b.put(position(2, 'c'), piece(piece::silver, piece::cat));

  step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'b'), north));

  ASSERT_TRUE(s);
  EXPECT_TRUE(s->capture());
  EXPECT_EQ("Eb3n Hc3x", s->to_string());
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
  EXPECT_EQ(expected_total, steps.size());

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

  EXPECT_EQ(expected_ordinary, ordinary_count);
  EXPECT_EQ(expected_push, push_count);
  EXPECT_EQ(expected_pull, pull_count);
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

TEST_F(move_generation, new_generator) {
  std::vector<step> steps = generate_steps(b, piece::gold);
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

struct expect_steps {
  std::set<step> expected;

  bool check(std::vector<step> const& result) {
    EXPECT_EQ(expected.size(), result.size());
    EXPECT_TRUE(expected == std::set<step>(result.begin(), result.end()));
    return
      result.size() == expected.size() &&
      std::set<step>(result.begin(), result.end()) == expected;
  }
};

expect_steps& operator , (expect_steps& e, std::string const& step) {
  e.expected.insert(e.expected.end(), *step::from_string(step));
  return e;
}

TEST(bit_step_generation, ordinary) {
  board b;
  b.put(position(4, 'd'), piece(piece::gold, piece::cat));
  expect_steps s;
  s, "Cd4n", "Cd4e", "Cd4w", "Cd4s";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));
}

TEST(bit_step_generation, correct_player) {
  board b;
  b.put(position(7, 'g'), piece(piece::silver, piece::camel));
  b.put(position(4, 'd'), piece(piece::gold, piece::cat));
  expect_steps s;
  s, "Cd4n", "Cd4e", "Cd4w", "Cd4s";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));

  s.expected.clear();
  s, "mg7n", "mg7e", "mg7s", "mg7w";

  EXPECT_TRUE(s.check(generate_steps(b, piece::silver)));
}

TEST(bit_step_generation, rabbit_movement) {
  board b;
  b.put(position(4, 'e'), piece(piece::gold, piece::rabbit));
  b.put(position(5, 'g'), piece(piece::silver, piece::rabbit));

  expect_steps s;
  s, "Re4n", "Re4e", "Re4w";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));

  s.expected.clear();
  s, "rg5s", "rg5e", "rg5w";

  EXPECT_TRUE(s.check(generate_steps(b, piece::silver)));
}

TEST(bit_step_generation, walls) {
  board b;
  b.put(position(1, 'c'), piece(piece::gold, piece::cat));
  b.put(position(8, 'c'), piece(piece::gold, piece::cat));
  b.put(position(6, 'h'), piece(piece::gold, piece::dog));
  b.put(position(6, 'a'), piece(piece::gold, piece::dog));
  b.put(position(1, 'a'), piece(piece::gold, piece::horse));
  b.put(position(1, 'h'), piece(piece::gold, piece::horse));
  b.put(position(8, 'a'), piece(piece::gold, piece::camel));
  b.put(position(8, 'h'), piece(piece::gold, piece::elephant));

  expect_steps s;
  s,
    "Ha1n", "Ha1e",
    "Cc1e", "Cc1w", "Cc1n",
    "Hh1n", "Hh1w",
    "Da6n", "Da6e", "Da6s",
    "Dh6n", "Dh6w", "Dh6s",
    "Ma8e", "Ma8s",
    "Cc8e", "Cc8w", "Cc8s",
    "Eh8w", "Eh8s";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));
}

TEST(bit_step_generation, block) {
  board b;
  b.put(position(1, 'g'), piece(piece::silver, piece::horse));
  b.put(position(1, 'h'), piece(piece::silver, piece::elephant));
  b.put(position(2, 'h'), piece(piece::silver, piece::horse));
  b.put(position(3, 'h'), piece(piece::silver, piece::dog));
  b.put(position(4, 'd'), piece(piece::silver, piece::cat));
  b.put(position(5, 'd'), piece(piece::silver, piece::cat));
  b.put(position(5, 'e'), piece(piece::silver, piece::dog));

  expect_steps s;
  s,
    "hg1w", "hg1n",
    "hh2w",
    "dh3w", "dh3n",
    "cd4e", "cd4w", "cd4s",
    "cd5w", "cd5n",
    "de5n", "de5s", "de5e";

  EXPECT_TRUE(s.check(generate_steps(b, piece::silver)));
}

TEST(bit_step_generation, freeze) {
  board b;
  b.put(position(1, 'g'), piece(piece::gold, piece::rabbit));
  b.put(position(2, 'g'), piece(piece::gold, piece::dog));
  b.put(position(2, 'h'), piece(piece::silver, piece::camel));
  b.put(position(4, 'd'), piece(piece::gold, piece::cat));
  b.put(position(4, 'e'), piece(piece::silver, piece::elephant));

  expect_steps s;
  s,
    "Rg1e", "Rg1w",
    "Dg2n", "Dg2w";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));
}

TEST(bit_step_generation, suicide) {
  board b;
  b.put(position(5, 'c'), piece(piece::gold, piece::cat));

  expect_steps s;
  s, "Cc5n Cc6x", "Cc5e", "Cc5w", "Cc5s";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));
}

TEST(bit_step_generation, abandonment) {
  board b;
  b.put(position(2, 'f'), piece(piece::gold, piece::rabbit));
  b.put(position(3, 'c'), piece(piece::gold, piece::cat));
  b.put(position(3, 'd'), piece(piece::gold, piece::rabbit));
  b.put(position(3, 'f'), piece(piece::gold, piece::elephant));
  b.put(position(3, 'g'), piece(piece::gold, piece::dog));

  expect_steps s;
  s,
    "Rf2e", "Rf2w",
    "Cc3n", "Cc3w", "Cc3s",
    "Rd3n Cc3x", "Rd3e Cc3x",
    "Ef3n", "Ef3w",
    "Dg3n", "Dg3e", "Dg3s";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));
}

TEST(bit_step_generation, pull_push) {
  board b;
  b.put(position(4, 'd'), piece(piece::gold, piece::elephant));
  b.put(position(4, 'e'), piece(piece::silver, piece::dog));

  b.put(position(5, 'g'), piece(piece::gold, piece::camel));
  b.put(position(5, 'h'), piece(piece::silver, piece::rabbit));

  b.put(position(8, 'g'), piece(piece::gold, piece::horse));
  b.put(position(8, 'h'), piece(piece::silver, piece::cat));

  b.put(position(7, 'b'), piece(piece::gold, piece::dog));
  b.put(position(5, 'b'), piece(piece::silver, piece::cat));
  b.put(position(6, 'a'), piece(piece::silver, piece::rabbit));
  b.put(position(6, 'b'), piece(piece::silver, piece::rabbit));
  b.put(position(6, 'c'), piece(piece::silver, piece::rabbit));
  b.put(position(7, 'a'), piece(piece::silver, piece::rabbit));
  b.put(position(7, 'c'), piece(piece::silver, piece::rabbit));
  b.put(position(8, 'a'), piece(piece::silver, piece::rabbit));
  b.put(position(8, 'b'), piece(piece::silver, piece::rabbit));
  b.put(position(8, 'c'), piece(piece::silver, piece::rabbit));

  b.put(position(7, 'd'), piece(piece::gold, piece::dog));
  b.put(position(7, 'e'), piece(piece::silver, piece::elephant));

  expect_steps s;
  s,
    "Ed4n", "Ed4n de4w", "Ed4w", "Ed4w de4w", "Ed4s", "Ed4s de4w",
    "de4n Ed4e", "de4e Ed4e", "de4s Ed4e",

    "Mg5n", "Mg5n rh5w", "Mg5w", "Mg5w rh5w", "Mg5s", "Mg5s rh5w",
    "rh5n Mg5e", "rh5s Mg5e",

    "Hg8s", "Hg8s ch8w", "Hg8w", "Hg8w ch8w",
    "ch8s Hg8e";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));
}

TEST(bit_step_generation, push_pull_capture) {
  board b;
  b.put(position(3, 'c'), piece(piece::gold, piece::rabbit));
  b.put(position(4, 'c'), piece(piece::gold, piece::elephant));
  b.put(position(5, 'c'), piece(piece::silver, piece::dog));

  b.put(position(3, 'f'), piece(piece::gold, piece::horse));
  b.put(position(3, 'g'), piece(piece::silver, piece::dog));
  b.put(position(4, 'f'), piece(piece::gold, piece::camel));
  b.put(position(5, 'f'), piece(piece::silver, piece::cat));
  b.put(position(6, 'g'), piece(piece::silver, piece::rabbit));

  expect_steps s;
  s,
    "Ec4e Rc3x", "Ec4w Rc3x",
    "dc5n dc6x Ec4n Rc3x", "dc5e Ec4n Rc3x", "dc5w Ec4n Rc3x",
    "Ec4w Rc3x dc5s", "Ec4e Rc3x dc5s",

    "Rc3e", "Rc3w",

    "Hf3s", "Hf3w",
    "Hf3s dg3w df3x", "Hf3w dg3w df3x",
    "dg3n Hf3e", "dg3e Hf3e", "dg3s Hf3e",

    "Mf4e Hf3x", "Mf4w Hf3x",
    "cf5n Mf4n Hf3x", "cf5e Mf4n Hf3x", "cf5w Mf4n Hf3x",
    "Mf4e Hf3x cf5s", "Mf4w Hf3x cf5s";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));
}

TEST(bit_step_generation, forced_abandonment) {
  board b;
  b.put(position(5, 'g'), piece(piece::gold, piece::dog));
  b.put(position(6, 'f'), piece(piece::silver, piece::cat));
  b.put(position(6, 'g'), piece(piece::silver, piece::rabbit));

  expect_steps s;
  s,
    "Dg5s", "Dg5e", "Dg5w",
    "Dg5s rg6s cf6x", "Dg5e rg6s cf6x", "Dg5w rg6s cf6x",
    "rg6n cf6x Dg5n", "rg6e cf6x Dg5n";

  EXPECT_TRUE(s.check(generate_steps(b, piece::gold)));
}

class apply_test : public testing::Test {
protected:
  board b;
  board original;
  board_masks m;
  board_masks original_mask;

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
    m = original_mask = masks_from_board(b);
  }
};

TEST_F(apply_test, ordinary_move) {
  step s = *step::validate_ordinary_step(b, elementary_step::displacement(position(1, 'c'), east));
  apply(s, b);
  EXPECT_FALSE(b.get(position(1, 'c')));
  ASSERT_TRUE(b.get(position(1, 'd')));
  EXPECT_EQ(b.get(position(1, 'd'))->type(), piece::cat);
  EXPECT_EQ(b.get(position(1, 'd'))->color(), piece::gold);

  apply(s, m);
  board_masks const correct = masks_from_board(b);
  EXPECT_TRUE(correct == m);

  unapply(s, b);
  EXPECT_TRUE(b == original);

  unapply(s, m);
  EXPECT_TRUE(original_mask == m);
}

TEST_F(apply_test, ordinary_suicide) {
  step s = *step::validate_ordinary_step(b, elementary_step::displacement(position(6, 'b'), east));
  apply(s, b);
  EXPECT_FALSE(b.get(position(6, 'b')));
  EXPECT_FALSE(b.get(position(6, 'c')));

  apply(s, m);
  EXPECT_TRUE(masks_from_board(b) == m);

  unapply(s, b);
  EXPECT_TRUE(b == original);

  unapply(s, m);
  EXPECT_TRUE(original_mask == m);
}

TEST_F(apply_test, pull) {
  step s = *step::validate_pull(b,
      elementary_step::displacement(position(5, 'e'), south),
      elementary_step::displacement(position(6, 'e'), south));
  apply(s, b);
  EXPECT_FALSE(b.get(position(6, 'e')));
  ASSERT_TRUE(b.get(position(5, 'e')));
  EXPECT_EQ(b.get(position(5, 'e'))->type(), piece::rabbit);
  ASSERT_TRUE(b.get(position(4, 'e')));
  EXPECT_EQ(b.get(position(4, 'e'))->type(), piece::horse);

  apply(s, m);
  EXPECT_TRUE(masks_from_board(b) == m);

  unapply(s, b);
  EXPECT_TRUE(b == original);

  unapply(s, m);
  EXPECT_TRUE(original_mask == m);
}

TEST_F(apply_test, push) {
  step s = *step::validate_push(b,
      elementary_step::displacement(position(7, 'g'), west),
      elementary_step::displacement(position(6, 'g'), north));
  apply(s, b);
  EXPECT_FALSE(b.get(position(6, 'g')));
  ASSERT_TRUE(b.get(position(7, 'f')));
  EXPECT_EQ(b.get(position(7, 'f'))->type(), piece::dog);
  ASSERT_TRUE(b.get(position(7, 'g')));
  EXPECT_EQ(b.get(position(7, 'g'))->type(), piece::elephant);

  apply(s, m);
  EXPECT_TRUE(masks_from_board(b) == m);

  unapply(s, b);
  EXPECT_TRUE(b == original);

  unapply(s, m);
  EXPECT_TRUE(original_mask == m);
}

TEST_F(apply_test, push_capture) {
  step s = *step::validate_push(b,
      elementary_step::displacement(position(3, 'g'), east),
      elementary_step::displacement(position(4, 'g'), south));
  apply(s, b);
  EXPECT_FALSE(b.get(position(4, 'g')));
  EXPECT_FALSE(b.get(position(3, 'f')));
  ASSERT_TRUE(b.get(position(3, 'g')));
  EXPECT_EQ(b.get(position(3, 'g'))->type(), piece::horse);

  apply(s, m);
  EXPECT_TRUE(masks_from_board(b) == m);

  unapply(s, b);
  EXPECT_TRUE(b == original);

  unapply(s, m);
  EXPECT_TRUE(original_mask == m);
}

TEST_F(apply_test, pull_capture) {
  step s = *step::validate_pull(b,
      elementary_step::displacement(position(3, 'c'), west),
      elementary_step::displacement(position(3, 'd'), west));
  apply(s, b);
  EXPECT_FALSE(b.get(position(3, 'c')));
  ASSERT_TRUE(b.get(position(3, 'b')));
  EXPECT_EQ(b.get(position(3, 'b'))->type(), piece::horse);

  apply(s, m);
  EXPECT_TRUE(masks_from_board(b) == m);

  unapply(s, b);
  EXPECT_TRUE(b == original);

  unapply(s, m);
  EXPECT_TRUE(original_mask == m);
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
  step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(position(2, 'b'), east));
  ASSERT_TRUE(s);
  EXPECT_EQ("cb2e", s->to_string());
  step_holder q = step::from_string("cb2e");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, suicide) {
  step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'g'), west));
  ASSERT_TRUE(s);
  EXPECT_EQ("Rg3w Rf3x", s->to_string());
  step_holder q = step::from_string("Rg3w Rf3x");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, push) {
  step_holder s = step::validate_push(b,
      elementary_step::displacement(position(5, 'c'), south),
      elementary_step::displacement(position(5, 'b'), east));
  ASSERT_TRUE(s);
  EXPECT_EQ("cc5s Hb5e", s->to_string());
  step_holder q = step::from_string("cc5s Hb5e");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, pull) {
  step_holder s = step::validate_pull(b,
      elementary_step::displacement(position(5, 'b'), north),
      elementary_step::displacement(position(5, 'c'), west));
  ASSERT_TRUE(s);
  EXPECT_EQ("Hb5n cc5w", s->to_string());
  step_holder q = step::from_string("Hb5n cc5w");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, capture) {
  step_holder s = step::validate_push(b,
      elementary_step::displacement(position(5, 'c'), north),
      elementary_step::displacement(position(5, 'b'), east));
  ASSERT_TRUE(s);
  EXPECT_EQ("cc5n cc6x Hb5e", s->to_string());
  step_holder q = step::from_string("cc5n cc6x Hb5e");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST_F(string_test, sacrifice) {
  step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(position(7, 'f'), north));
  ASSERT_TRUE(s);
  EXPECT_EQ("Rf7n Ef6x", s->to_string());
  step_holder q = step::from_string("Rf7n Ef6x");
  ASSERT_TRUE(q);
  EXPECT_EQ(*s, *q);
}

TEST(revalidate_test, identity) {
  board b;
  b.put(position(3, 'd'), piece(piece::gold, piece::dog));
  step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'd'), north));
  ASSERT_TRUE(s);

  bool const valid = revalidate(*s, b, piece::gold);
  EXPECT_TRUE(valid);
}

TEST(revalidate_test, different_piece) {
  board b;
  b.put(position(3, 'd'), piece(piece::gold, piece::dog));
  step_holder s = step::validate_ordinary_step(
    b, elementary_step::displacement(position(3, 'd'), north)
  );
  ASSERT_TRUE(s);

  b.remove(position(3, 'd'));
  b.put(position(3, 'd'), piece(piece::gold, piece::elephant));
  
  step_holder result = revalidate(*s, b, piece::gold);
  ASSERT_FALSE(result);
  //EXPECT_EQ("Ed3n", result->to_string());
}

TEST(revalidate_test, no_capture) {
  board b;
  b.put(position(3, 'd'), piece(piece::gold, piece::dog));
  b.put(position(3, 'c'), piece(piece::gold, piece::rabbit));

  step_holder s = step::validate_ordinary_step(
    b, elementary_step::displacement(position(3, 'd'), north)
  );
  ASSERT_TRUE(s);
  EXPECT_TRUE(s->capture());

  b.remove(position(3, 'c'));
  step_holder result = revalidate(*s, b, piece::gold);
  ASSERT_TRUE(result);
  EXPECT_EQ("Dd3n", result->to_string());
}

TEST(revalidate_test, different_player) {
  board b;
  b.put(position(3, 'd'), piece(piece::gold, piece::dog));
  step_holder s = step::validate_ordinary_step(b, elementary_step::displacement(position(3, 'd'), north));
  ASSERT_TRUE(s);

  bool const valid = revalidate(*s, b, piece::silver);
  EXPECT_FALSE(valid);
}

TEST(revalidate_test, pull) {
  board b;
  b.put(position(3, 'd'), piece(piece::gold, piece::dog));
  b.put(position(4, 'd'), piece(piece::silver, piece::elephant));
  step_holder s = step::validate_pull(
    b,
    elementary_step::displacement(position(4, 'd'), north),
    elementary_step::displacement(position(3, 'd'), north)
  );
  ASSERT_TRUE(s);

  b.put(position(6, 'h'), piece(piece::gold, piece::rabbit));

  bool const valid = revalidate(*s, b, piece::silver);
  EXPECT_TRUE(valid);
}

TEST(revalidate, push_capture) {
  board b;
  b.put(position(7, 'c'), piece(piece::gold, piece::rabbit));
  b.put(position(7, 'b'), piece(piece::silver, piece::horse));

  step_holder s = step::validate_push(
    b,
    elementary_step::displacement(position(7, 'c'), south),
    elementary_step::displacement(position(7, 'b'), east)
  );
  ASSERT_TRUE(s);

  bool const valid = revalidate(*s, b, piece::silver);
  EXPECT_TRUE(valid);
}

TEST(revalidate, frozen) {
  board b;
  b.put(position(3, 'd'), piece(piece::gold, piece::dog));
  step_holder s = step::validate_ordinary_step(
    b, elementary_step::displacement(position(3, 'd'), east)
  );

  ASSERT_TRUE(s);

  b.put(position(4, 'd'), piece(piece::silver, piece::elephant));
  step_holder result = revalidate(*s, b, piece::gold);
  EXPECT_FALSE(result);
}

TEST(revalidate, cant_move) {
  board b;
  b.put(position(3, 'd'), piece(piece::gold, piece::dog));
  step_holder s = step::validate_ordinary_step(
    b, elementary_step::displacement(position(3, 'd'), north)
  );

  ASSERT_TRUE(s);

  b.put(position(4, 'd'), piece(piece::gold, piece::cat));
  step_holder result = revalidate(*s, b, piece::gold);
  EXPECT_FALSE(result);
}

TEST(holder, comparisons) {
  step_holder a, b;
  EXPECT_TRUE(a == b);
  EXPECT_FALSE(a != b);

  b = step_holder::none;
  EXPECT_TRUE(a == b);
  EXPECT_FALSE(a != b);

  board pos;
  pos.put(position(1, 'a'), piece(piece::gold, piece::camel));
  a = step::validate_ordinary_step(
    pos,
    elementary_step::displacement(position(1, 'a'), north)
  );
  ASSERT_TRUE(a);

  EXPECT_FALSE(a == b);
  EXPECT_TRUE(a != b);

  b = a;
  EXPECT_TRUE(a == b);
  EXPECT_FALSE(a != b);

  b = step::validate_ordinary_step(
    pos,
    elementary_step::displacement(position(1, 'a'), north)
  );
  EXPECT_TRUE(a == b);
  EXPECT_FALSE(a != b);
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
