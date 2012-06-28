#include "movement.hpp"
#include "board.hpp"
#include "piece.hpp"

#include <boost/bind.hpp>
#include <boost/array.hpp>

#include <sstream>
#include <stdexcept>
#include <cctype>

namespace {

//! Is a stronger than b? (Their colour does not matter.)
bool stronger(apns::piece a, apns::piece b) {
  using namespace apns;
  return index_from_type(a.type()) < index_from_type(b.type());
}

/*
 * Is there a piece of the given colour adjacent to the specified position?
 *
 * \param where Neighbourhood of this position will be considered. Position 
 *   referred to by where may be empty.
 * \param color What color to search for.
 * \param board Board with the pieces on it.
 */
bool colour_adjacent(apns::position where, apns::piece::color_t color,
                     apns::board const& board) {
  using namespace apns;
  return std::find_if(
      adjacent_pieces_begin(board, where), adjacent_pieces_end(),
      boost::bind(&piece::color, _1) == color)
    != adjacent_pieces_end();
}

/**
 * Is there an friendly piece adjacent to the given one?
 *
 * \param where Where is the given piece. This position must be nonempty.
 * \param board Board with the pieces.
 */
bool friendly_adjacent(apns::position where, apns::board const& board) {
  using namespace apns;
  piece const what = *board.get(where);  // Assuming the position is non-empty.
  return colour_adjacent(where, what.color(), board);
}

/**
 * Is there a stronger opponent piece adjacent to the given one?
 *
 * \param where Where is the given piece. This position must be nonempty.
 * \param board Board with the pieces.
 */
bool stronger_opponent_adjacent(apns::position where, 
                                apns::board const& board) {
  using namespace apns;
  piece const what = *board.get(where);  // Assuming the position is non-empty.

  return std::find_if(
      adjacent_pieces_begin(board, where), adjacent_pieces_end(),
      boost::bind(&piece::color, _1) == opponent_color(what.color())
        && boost::bind(stronger, _1, what))
    != adjacent_pieces_end();
}

/**
 * Decide whether there would be a capture if a certain move were performed.
 *
 * The hypothetical move is #what moving from #old_position to #new_position. 
 * #old_position is thus assumed to be empty by this function, while 
 * #new_position is assumed to contain #what. (It doesn't matter if this 
 * position actually contains any other piece on the board.)
 *
 * \return Would #what be captured after such move?
 */
bool would_be_capture(
  apns::piece what, apns::position old_position, apns::position new_position, 
  apns::board const& board
) {
  using namespace apns;
  if (trap(new_position)) {
    for (neighbourhood_iter neighbour = neighbourhood_begin(new_position);
         neighbour != neighbourhood_end(); ++neighbour) {
      // Is there a friendly piece adjacent?
      if (*neighbour != old_position    // Old position is assumed to be empty.
          && !empty(*neighbour, board)
          && board.get(*neighbour)->color() == what.color()) {
        return false;
      }
    }

    return true;
  } else {
    return false;
  }
}

/**
 * Decide whether there would be a capture if a certain move were performed.
 *
 * The hypothetical move is #what moving from position #from to position 
 * #where. The question now is whether the piece standing on #pos would be 
 * captured after such move.
 *
 * \return Would piece from #pos be captured?
 */
bool would_be_capture(
  apns::position pos, apns::piece what, apns::position from,
  apns::position where, apns::board const& board
) {
  using namespace apns;
  if (!empty(pos, board)
      && trap(pos)) {
    piece const target = *board.get(pos);

    for (neighbourhood_iter neighbour = neighbourhood_begin(pos);
         neighbour != neighbourhood_end(); ++neighbour) {
      if (*neighbour != from &&
          *neighbour != where &&
          !empty(*neighbour, board) &&
          board.get(*neighbour)->color() == target.color()) {
        return false;
      } else if (*neighbour == where
          && target.color() == what.color()) {
        return false;
      }
    }

    return true;
  } else {
    return false;
  }
}

typedef std::vector<apns::elementary_step> elementary_steps_cont;

/**
 * Check for captures and insert them into a sequence of elementary steps.
 *
 * Assuming that the piece #what has moved from position #from to position 
 * #destination, this function calculates all captures that would have resulted 
 * from this move and inserts them into #sequence.
 *
 * \param what What piece has moved.
 * \param from From where it has moved.
 * \param destination To where it has moved.
 * \param board Contains the pieces.
 * \param sequence All detected captures will be inserted here.
 */
void check_for_captures(
  apns::piece what, apns::position from, apns::position destination,
  apns::board const& board, elementary_steps_cont& sequence
) {
  using namespace apns;
  if (would_be_capture(what, from, destination, board)) {
    elementary_step capture = elementary_step::make_capture(destination);
    capture.set_what(what);
    sequence.insert(sequence.end(), capture);
  } else {
    for (neighbourhood_iter neighbour = neighbourhood_begin(from);
         neighbour != neighbourhood_end(); ++neighbour) {
      if (would_be_capture(*neighbour, what, from, destination, board)) {
        elementary_step capture = elementary_step::make_capture(*neighbour);
        capture.set_what(*board.get(*neighbour));
        sequence.insert(sequence.end(), capture);
      }
    }
  }
}

//! Get the inverse direction to the given one.
apns::direction inverse_dir(apns::direction dir) {
  using namespace apns;
  switch (dir) {
  case north:       return south;
  case east:        return west;
  case south:       return north;
  case west:        return east;
  default:          assert(!"Never gets here");
  }

  return north;
}

//! Convert a string description of an elementary step into a real elementary 
//! step, or nothing if the description isn't valid.
boost::optional<apns::elementary_step> 
elementary_step_from_string(std::string const& string) {
  using namespace apns;

  if (string.length() == 4) {  // Each elementary step is exactly four 
                               // characters long.

    boost::optional<piece> maybe_piece = piece_from_letter(string[0]);
    position::col_t const column = string[1];
    position::row_t const row = string[2] - '0';
    char const dir_or_capture = string[3];

    if (maybe_piece &&
        position::MIN_COLUMN <= column &&
        column <= position::MAX_COLUMN &&
        position::MIN_ROW <= row &&
        row <= position::MAX_ROW) {
      piece const piece = *maybe_piece;
      position const position = apns::position(row, column);

      if (dir_or_capture == 'x') {  // A capture.
        return elementary_step::make_capture(position, piece);
      } else {
        direction dir;
        switch (dir_or_capture) {
        case 'n': dir = north;  break;
        case 'e': dir = east;   break;
        case 's': dir = south;  break;
        case 'w': dir = west;   break;
        default:
          return boost::optional<elementary_step>();
        }

        return elementary_step::displacement(position, dir, piece);
      }
    }
  }

  return boost::optional<elementary_step>();
}

//! Convert 'n', 's', 'e' or 'w' to the apropriate direction.
apns::direction dir_from_letter(char letter) {
  using namespace apns;
  switch (letter) {
  case 'n':     return north;
  case 'e':     return east;
  case 's':     return south;
  case 'w':     return west;
  default:      assert(!"Never gets here");
  }
  return north;
}

// Indexes into elementary_step::representation.
std::size_t const PIECE_INDEX = 0;
std::size_t const COLUMN_INDEX = 1;
std::size_t const ROW_INDEX = 2;
std::size_t const DIR_INDEX = 3;

} // Anonymous namespace.

namespace apns {

static boost::array<int, types_array_t::static_size> const TYPE_COST = {{
  10,     // elephant
  7,      // camel
  5,      // horse
  3,      // dog
  2,      // cat
  1       // rabbit
}};

static int const CAPTURE_COEF = 4;
static int const PUSH_PULL_COEF = 2;
static int const TRAP_ABANDONMENT_PENALTY = 1;

static int rabbit_score(piece::color_t color, position pos) {
  if (color == piece::gold) {
    if (pos.row() >= 5)
      return pos.row() - position::MIN_ROW;
  } else {
    if (pos.row() <= 4)
      return position::MAX_ROW - pos.row();
  }

  return TYPE_COST[index_from_type(piece::rabbit)];
}

static int cost(piece what, position where) {
  if (what.type() != piece::rabbit)
    return TYPE_COST[index_from_type(what.type())];
  else
    return rabbit_score(what.color(), where);
}

/// Check for captures resulting from what moving from src to dst.
/// \return Score of the captured pieces. Captured pieces of the player who
///         owns the moving pieces are counted with negative sign, opponent's
///         with positive sign.
template <typename OutIter>
int check_captures(position src, position dst, board const& board,
                   piece what, OutIter out) {
  std::size_t const player_index = index_from_color(what.color());
  board::mask supporters;

  int score = 0;

  if (trap(dst)) {
    supporters = neighbourhood(dst) & board.players()[player_index];
    supporters[src] = false;

    // Check for piece stepping into a trap.
    if (supporters.empty()) {
      *out++ = elementary_step::make_capture(dst, what);
      score -= CAPTURE_COEF * cost(what, src);
    }
  }

  // Check for piece abandoning another one standing on a trap.
  if (neighbourhood(src) & board::mask::TRAPS && !trap(dst)) {
    position const trap =
      (neighbourhood(src) & board::mask::TRAPS).first_set();
    boost::optional<piece> const trapped = board.get(trap);
    if (trapped) {
      std::size_t trapped_player_index = index_from_color(trapped->color());
      supporters = neighbourhood(trap) & board.players()[trapped_player_index];
      supporters[src] = false;

      if (supporters.empty()) {
        *out++ = elementary_step::make_capture(trap, *trapped);
        score +=
          (trapped->color() == what.color() ? -1 : +1) *
          CAPTURE_COEF * cost(*trapped, trap);
      }
    }
  }

  return score;
}

template <typename OutIter>
int make_displacement(board const& board, position src, position dst, piece what, OutIter out) {
  int score = 0;

  *out++ = elementary_step::displacement(
    src, adjacent_dir(src, dst), what
  );

  if (what.type() == piece::rabbit)
    score += rabbit_score(what.color(), dst);

  score += check_captures(src, dst, board, what, out);

  return score;
}

position elementary_step::from() const {
  return position(representation_[ROW_INDEX] - '0', representation_[COLUMN_INDEX]);
}

direction elementary_step::where() const {
  return dir_from_letter(representation_[DIR_INDEX]);
}

bool elementary_step::capture() const {
  return representation_[DIR_INDEX] == 'x';
}

boost::optional<piece> elementary_step::what() const {
  assert(representation_[PIECE_INDEX] != 0);
  if (representation_[PIECE_INDEX] != ' ')
    return piece_from_letter_unsafe(representation_[PIECE_INDEX]);
  else
    return boost::none;
}

std::string elementary_step::to_string() const {
  return std::string(representation_.begin(), representation_.end());
}

void elementary_step::set_what(boost::optional<piece> new_what) {
  if (new_what) {
    representation_[PIECE_INDEX] = letter_from_piece(*new_what);
  } else {
    representation_[PIECE_INDEX] = ' ';
  }

  assert(representation_[PIECE_INDEX] != 0);
}

bool elementary_step::equal(elementary_step const& other) const {
  return std::equal(representation_.begin(), representation_.end(),
                    other.representation_.begin());
}

elementary_step elementary_step::displacement(position from, direction where, boost::optional<piece> what) {
  return elementary_step(from, where, what);
}

elementary_step elementary_step::make_capture(position from, boost::optional<piece> what) {
  return elementary_step(from, what);
}

elementary_step::elementary_step(position from, direction where, boost::optional<piece> what) {
  representation_[ROW_INDEX] = '0' + from.row();
  representation_[COLUMN_INDEX] = from.column();
  representation_[DIR_INDEX] = letter_from_direction(where);
  set_what(what);

  assert(!invalid());
}

elementary_step::elementary_step(position which, boost::optional<piece> what) {
  representation_[ROW_INDEX] = '0' + which.row();
  representation_[COLUMN_INDEX] = which.column();
  representation_[DIR_INDEX] = 'x';
  set_what(what);

  assert(!invalid());
}

bool operator == (elementary_step const& lhs, elementary_step const& rhs) {
  return lhs.equal(rhs);
}

bool operator != (elementary_step const& lhs, elementary_step const& rhs) {
  return !operator == (lhs, rhs);
}

step_holder step::validate_ordinary_step(board const& board, elementary_step step) {
  position const from    = step.from();
  direction const where  = step.where();

  if (adjacent_valid(from, where)
      && !apns::empty(from, board)
      && !frozen(from, board)
      && apns::empty(make_adjacent(from, where), board)) {
    position const destination  = make_adjacent(from, where);
    piece const what            = *board.get(from);

    if (what.type() == piece::rabbit
        && ((what.color() == piece::gold && where == south)
            || (what.color() == piece::silver && where == north))) {
      // Rabbits can't go backwards.
      return step_holder::none;
    }

    elementary_step_seq sequence;
    step.set_what(board.get(from));
    sequence.insert(sequence.begin(), step);

    check_for_captures(what, from, destination, board, sequence);

    return step_holder(apns::step(sequence.begin(), sequence.end()));
  } else {
    return step_holder::none;
  }
}

step_holder step::validate_push(board const& board,
    elementary_step const& first_step, elementary_step const& second_step) {
  if (adjacent(first_step.from(), second_step.from())
      && !apns::empty(first_step.from(), board)
      && !apns::empty(second_step.from(), board)
      && stronger(
           *board.get(second_step.from()), 
           *board.get(first_step.from())
         )
      && board.get(second_step.from())->color() ==
           opponent_color(board.get(first_step.from())->color())
      && adjacent_valid(first_step.from(), first_step.where())
      && adjacent_valid(second_step.from(), second_step.where())
      && make_adjacent(second_step.from(), second_step.where()) == 
           first_step.from()
      && apns::empty(
           make_adjacent(first_step.from(), first_step.where()), 
           board
         )
      && !frozen(second_step.from(), board)) {
    return make_push_pull(board, first_step, second_step);
  } else {
    return step_holder::none;
  }
}

step_holder step::validate_pull(board const& board,
    elementary_step const& first_step, elementary_step const& second_step) {
  if (adjacent(first_step.from(), second_step.from())
      && !apns::empty(first_step.from(), board)
      && !apns::empty(second_step.from(), board)
      && stronger(
           *board.get(first_step.from()), 
           *board.get(second_step.from())
         )
      && board.get(first_step.from())->color() ==
           opponent_color(board.get(second_step.from())->color())
      && adjacent_valid(first_step.from(), first_step.where())
      && adjacent_valid(second_step.from(), second_step.where())
      && make_adjacent(second_step.from(), second_step.where()) == 
           first_step.from()
      && apns::empty(
           make_adjacent(first_step.from(), first_step.where()), 
           board
         )
      && !frozen(first_step.from(), board)
      ) {
    return make_push_pull(board, first_step, second_step);
  } else {
    return step_holder::none;
  }
}

step_holder step::from_string(std::string const& string) {
  // Split the input up into elementary steps separated by spaces. Convert each 
  // elementary step separately.

  elementary_step_seq elementary_steps;
  std::string el_step_description;
  std::istringstream input(string);
  while (std::getline(input, el_step_description, ' ')) {
    boost::optional<elementary_step> el_step = 
      elementary_step_from_string(el_step_description);

    if (el_step) {
      elementary_steps.insert(elementary_steps.end(), *el_step);
    } else {
      return step_holder::none;
    }
  }

  if (elementary_steps.size() <= MAX_EL_STEPS) {
    return step_holder(step(elementary_steps.begin(), elementary_steps.end()));
  }

  return step_holder::none;
}

bool step::capture() const {
  for (iterator el_step = begin(); el_step != end(); ++el_step)
    if (el_step->capture())
      return true;
  return false;
}

std::string step::to_string() const {
  std::string result;
  for (iterator es = begin(); es != end(); ++es) {
    if (es != begin())
      result += ' ';
    result += es->to_string();
  }

  return result;
}

step::iterator step::begin() const {
  return representation_.begin();
}

step::iterator step::end() const {
  return std::find_if(representation_.begin(), representation_.end(),
                      boost::bind(&elementary_step::invalid, _1));
}

step::reverse_iterator step::rbegin() const {
  return reverse_iterator(end());
}

step::reverse_iterator step::rend() const {
  return reverse_iterator(begin());
}

int step::steps_used() const {
  return std::count_if(begin(), end(), !boost::bind(&elementary_step::capture, _1));
}

bool step::moves_rabbit() const {
  return
    std::find_if(
      begin(), end(),
      boost::bind(
        &piece::type,
        boost::bind(
          static_cast<piece const& (boost::optional<piece>::*)() const>(&boost::optional<piece>::get),
          boost::bind(
            &elementary_step::what, _1
          )
        )
      ) == piece::rabbit
    ) != end();
}

step step::make_push_pull(board const& board, elementary_step first_step,
                          elementary_step second_step) {
  elementary_step_seq sequence;
  piece const first_piece = *board.get(first_step.from());
  first_step.set_what(first_piece);
  sequence.insert(sequence.begin(), first_step);

  position const first_destination = make_adjacent(first_step.from(), first_step.where());

  check_for_captures(first_piece, first_step.from(), first_destination, board, sequence);

  piece const second_piece = *board.get(second_step.from());
  second_step.set_what(second_piece);
  sequence.insert(sequence.end(), second_step);

  position const second_destination = make_adjacent(second_step.from(), second_step.where());

  check_for_captures(second_piece, second_step.from(), second_destination, board, sequence);

  return step(sequence.begin(), sequence.end());
}

step_holder step_holder::none;

bool operator == (step const& lhs, step const& rhs) {
  step::iterator left = lhs.begin();
  step::iterator right = rhs.begin();

  for (; left != lhs.end() && right != rhs.end(); ++left, ++right)
    if (*left != *right)
      return false;

  return left == lhs.end() && right == rhs.end();
}

bool operator != (step const& lhs, step const& rhs) {
  return !operator == (lhs, rhs);
}

e_step_kind step_kind(step const& step, piece::color_t player, 
                      board const& board) {
  if (step.steps_used() == 1)
    return ordinary;
  else if (board.get(step.begin()->from())->color() == player)
    return pull;
  else
    return push;
}

namespace {

//! Attempt to apply an elementary step to the board. If possible, returns 
//! true; otherwise returns false and the board is left unmodified.
bool apply_elementary(elementary_step const& es, board& board) {
  boost::optional<piece> maybe_what = board.get(es.from());
  if (maybe_what) {
    piece const what = *maybe_what;
    board.remove(es.from());

    if (!es.capture()) {
      position const target_pos = make_adjacent(es.from(), es.where());
      if (!board.get(target_pos)) {
        board.put(target_pos, what);
      }
      else {
        // Target position is not empty -- can't apply. But we've already 
        // removed a piece -- we need to put it back.
        board.put(es.from(), what);
        return false;
      }
    }

    return true;
  } else {
    return false;
  }
}

//! Attempt to undo the application of an elementary step to the board. If 
//! possible, returns true; otherwise returns false and the board is left 
//! unmodified.
bool unapply_elementary(elementary_step const& es, board& board) {
  position const original_pos = es.from();

  if (!es.capture()) {
    assert(adjacent_valid(original_pos, es.where()));
    position const destination = make_adjacent(original_pos, es.where());

    boost::optional<piece> maybe_what = board.get(destination);
    if (maybe_what) {
      piece const what = *maybe_what;
      board.remove(destination);

      if (!board.get(original_pos)) {
        board.put(original_pos, what);
      } else {
        // Unapply failed -- revert.
        board.put(destination, what);
        return false;
      }
      return true;
    } else {
      return false;
    }
  } else if (!board.get(original_pos)) {
    assert(es.what());
    piece const what = *es.what();
    board.put(original_pos, what);
    return true;
  } else {
    return false;
  }
}

}  // namespace apns::<anonymous>

void apply(step const& step, board& board) {
  if (!try_apply(step, board))
    throw std::logic_error(
      "apply: Given step doesn't correspond to the given board"
    );
}

bool try_apply(step const& step, board& board) {
  for (apns::step::iterator es = step.begin();
       es != step.end(); ++es) {
    if (!apply_elementary(*es, board)) {
      // The application has failed. We now need to roll back everything 
      // that's been done.
      if (es != step.begin()) {
        do {
          --es;
          unapply_elementary(*es, board);
        } while (es != step.begin());
      }

      return false;
    }
  }

  return true;
}

void unapply(step const& step, board& board) {
  if (!try_unapply(step, board))
    throw std::logic_error(
      "unapply: given step doesn't correspond to given board"
    );
}

bool try_unapply(step const& step, board& board) {
  for (apns::step::reverse_iterator es = step.rbegin();
       es != step.rend(); ++es) {
    if (!unapply_elementary(*es, board)) {
      // It's failed. That means we need to turn back the board to its original 
      // state.

      if (es != step.rbegin()) {
        do {
          --es;
          apply_elementary(*es, board);
        } while (es != step.rbegin());
      }

      return false;
    }
  }

  return true;
}

step_holder revalidate(
  step const& step, board const& board, piece::color_t player
) {
  step::iterator second_noncapture = std::find_if(
    boost::next(step.begin()), step.end(),
    !boost::bind(&elementary_step::capture, _1)
  );

  step_holder new_step;

  if (board.get(step.begin()->from())) {
    switch (step_kind(step, player, board)) {
    case ordinary: {
      new_step = step::validate_ordinary_step(board, *step.begin());
      if (new_step) {
        step::iterator first = new_step->begin();
        assert(first->what());

        if (first->what()->color() != player)
          return step_holder::none;
      }
    } break;

    case push: {
      assert(second_noncapture != step.end());
      new_step = step::validate_push(board, *step.begin(),
                                     *second_noncapture);
    } break;

    case pull: {
      assert(second_noncapture != step.end());
      new_step = step::validate_pull(board, *step.begin(),
                                     *second_noncapture);
    } break;
    }

    if (new_step) {
      step::iterator new_second_noncapture = std::find_if(
        boost::next(new_step->begin()), new_step->end(),
        !boost::bind(&elementary_step::capture, _1)
      );

      if (new_step->begin()->what() == step.begin()->what()) {
        if (second_noncapture != step.end()) {
          if (new_second_noncapture != step.end() &&
              second_noncapture->what() == new_second_noncapture->what())
            return new_step;
        } else
          return new_step;
      }
    }
  }

  return step_holder::none;
}

bool frozen(position position, board const& board) {
  if (!empty(position, board)) {
    return 
      stronger_opponent_adjacent(position, board) && 
      !friendly_adjacent(position, board);
  } else {
    throw std::logic_error("frozen: Specified position is empty");
  }
}

bool mobile(position position, board const& board) {
  return !frozen(position, board);
}

namespace {

template <typename Iter>
void apply(Iter begin, Iter end, board_masks& masks) {
  for (Iter s = begin; s != end; ++s) {
    board::mask const from(s->from());
    piece const what = *s->what();
    board::mask const update =
      !s->capture()
        ? from ^ from.shift(s->where())
        : from;

    masks.occupied ^= update;
    masks.players[index_from_color(what.color())] ^= update;
    masks.types[index_from_type(what.type())] ^= update;
  }
}

}

void apply(step const& step, board_masks& masks) {
  apply(step.begin(), step.end(), masks);
}

void unapply(step const& step, board_masks& masks) {
  apply(step.rbegin(), step.rend(), masks);
}

steps_cont generate_steps(board const& board, piece::color_t player) {
  typedef std::pair<int, step> scored_step;
  std::vector<scored_step> steps;
  piece::color_t const opponent = opponent_color(player);

  std::size_t const player_index = index_from_color(player);
  std::size_t const opponent_index = index_from_color(opponent);

  board::mask const have_friend =
    board.players()[player_index] &
    (board.players()[player_index].shift(north) |
     board.players()[player_index].shift(south) |
     board.players()[player_index].shift(east) |
     board.players()[player_index].shift(west));

  for (types_array_t::const_iterator type = TYPES.begin(); type != TYPES.end(); ++type) {
    std::size_t const type_index = index_from_type(*type);
    board::mask opponent_stronger, opponent_weaker;

    for (types_array_t::const_iterator op_type = TYPES.begin(); op_type != TYPES.end(); ++op_type) {
      std::size_t const op_type_index = index_from_type(*op_type);

      if (op_type < type)
        opponent_stronger |= board.players()[opponent_index] & board.types()[op_type_index];
      else if (op_type > type)
        opponent_weaker |= board.players()[opponent_index] & board.types()[op_type_index];
    }

    board::mask const pieces = board.players()[player_index] & board.types()[type_index];
    board::mask const stronger_opponent_adjacent =
      pieces &
        (opponent_stronger.shift(north) |
         opponent_stronger.shift(south) |
         opponent_stronger.shift(east) |
         opponent_stronger.shift(west));

    board::mask const unfrozen = pieces & (~stronger_opponent_adjacent | have_friend);

    for (board::mask::iterator pos = unfrozen.begin(); pos != unfrozen.end(); ++pos) {
      board::mask const vacant = neighbourhood(*pos) & ~board.occupied();
      board::mask const pullable = neighbourhood(*pos) & opponent_weaker;
      board::mask const forbidden =
        *type == piece::rabbit
          ? board::mask::row(pos->row()).shift(
              player == piece::gold ? south : north
            )
          : board::mask();
      board::mask const targets = vacant & ~forbidden;

      // Ordinary steps and pulls:

      for (board::mask::iterator dir = targets.begin(); dir != targets.end(); ++dir) {
        std::vector<elementary_step> ordinary;
        int score = make_displacement(
          board, *pos, *dir, piece(player, *type),
          std::back_inserter(ordinary)
        );

        steps.push_back(std::make_pair(score, step(ordinary.begin(), ordinary.end())));
        for (board::mask::iterator pullee = pullable.begin(); pullee != pullable.end(); ++pullee) {
          boost::optional<piece> const p = board.get(*pullee);
          assert(p);
          assert(p->color() != player);

          std::vector<elementary_step> pull(ordinary);
          score -= make_displacement(
            board, *pullee, *pos, *p,
            std::back_inserter(pull)
          );

          score += PUSH_PULL_COEF * cost(*p, *pos);

          steps.push_back(std::make_pair(
            score, step(pull.begin(), pull.end())
          ));
        }
      }

      // Pushes:

      board::mask const& pushable = pullable;

      for (board::mask::iterator pushee = pushable.begin(); pushee != pushable.end(); ++pushee) {
        board::mask const pushee_tgts = neighbourhood(*pushee) & ~board.occupied();
        for (board::mask::iterator push_tgt = pushee_tgts.begin(); push_tgt != pushee_tgts.end(); ++push_tgt) {
          std::vector<elementary_step> push;
          int score = 0;
          boost::optional<piece> const p = board.get(*pushee);
          assert(p);
          assert(p->color() != player);

          score -= make_displacement(
            board, *pushee, *push_tgt, *p,
            std::back_inserter(push)
          );
          score += make_displacement(
            board, *pos, *pushee, piece(player, *type),
            std::back_inserter(push)
          );
          score += PUSH_PULL_COEF * cost(*p, *push_tgt);

          steps.push_back(std::make_pair(
            score, step(push.begin(), push.end())
          ));
        }
      }
    }
  }

  std::sort(
    steps.begin(), steps.end(),
    boost::bind(&scored_step::first, _1) > boost::bind(&scored_step::first, _2)
  );

  steps_cont result;
  result.reserve(steps.size());
  std::transform(steps.begin(), steps.end(),
                 std::back_inserter(result),
                 boost::bind(&scored_step::second, _1));

  return result;
}

steps_iter::steps_iter()
  : board_(0)
  , piece_pos_(1, 'a')  // Need to init piece_pos somehow.
  , first_dir_(directions_end())
  , second_dir_(directions_end())
{
  assert(!board_);
}

steps_iter::steps_iter(position what_piece, apns::board const& board)
  : board_(&board)
  , piece_pos_(what_piece)
  , first_dir_(directions_begin())
  , second_dir_(directions_end())
  , state(ordinary)
{
  assert(this->board_);

  while (first_dir_ != directions_end() && 
         !adjacent_valid(piece_pos_, *first_dir_)) {
    ++first_dir_;
  }

  generate_ordinary_or_push();

  if (board_ != 0 && !result_) {
    increment();
  }

  assert(result_ || !board_);
}

step steps_iter::dereference() const {
  assert(board_);
  assert(result_);

  return *result_;
}

bool steps_iter::equal(steps_iter const& other) const {
  return
    (board_ == 0 && other.board_ == 0)
    || (board_ == other.board_
        && piece_pos_ == other.piece_pos_
        && first_dir_ == other.first_dir_
        && second_dir_ == other.second_dir_);
}

void steps_iter::increment() {
  assert(board_);

  do {
    do_increment();
  } while (board_ != 0 && !result_);

  assert(result_ || !board_);
}

void steps_iter::do_increment() {
  assert(board_);

  switch (state) {
    case ordinary: {
      // target_dir points to an empty position and we have generated the 
      // ordinary move already. Generate pulls then.
      state = pull;
    }

    // FALLTHROUGH

    case pull: {
      advance_second_to_weaker();

      if (second_dir_ == directions_end()) {
        // There is no weaker enemy adjacent. Let's go to the next direction.
        state = ordinary;
        advance_first();
        generate_ordinary_or_push();
      } else {
        // There is a weaker enemy adjacent, and second_dir is pointing to it. 
        // Reverse direction to second_dir, then, is the direction in which it 
        // will move when pulled.
        result_ = step::validate_pull(
          *board_,
          elementary_step::displacement(piece_pos_, *first_dir_),
          elementary_step::displacement(
            make_adjacent(piece_pos_, *second_dir_),
            inverse_dir(*second_dir_)
          )
        );
      }
    } break;

    case push: {
      generate_push();
    } break;
  }
}

void steps_iter::advance_first() {
  assert(board_);
  ++first_dir_;
  while (first_dir_ != directions_end() && 
         !adjacent_valid(piece_pos_, *first_dir_)) {
    ++first_dir_;
  }
}

void steps_iter::advance_second_to_empty() {
  assert(board_);
  position const second_center = make_adjacent(piece_pos_, *first_dir_);

  if (second_dir_ == directions_end()) {
    second_dir_ = directions_begin();
  } else {
    ++second_dir_;
  }

  while (second_dir_ != directions_end()
      && (!adjacent_valid(second_center, *second_dir_)
          || !empty(make_adjacent(second_center, *second_dir_), *board_))) {
    ++second_dir_;
  }
}

void steps_iter::advance_second_to_weaker() {
  assert(board_);
  position const second_center = piece_pos_;

  if (second_dir_ == directions_end()) {
    second_dir_ = directions_begin();
  } else {
    ++second_dir_;
  }

  while (second_dir_ != directions_end()) {
    piece const first_piece = *board_->get(piece_pos_);

    if (adjacent_valid(second_center, *second_dir_)
        && !apns::empty(make_adjacent(second_center, *second_dir_), *board_)) {
      piece const second_piece =
        *board_->get(make_adjacent(second_center, *second_dir_));

      if (second_piece.color() == opponent_color(first_piece.color())
          && stronger(first_piece, second_piece)) {
        return;
      } else {
        ++second_dir_;
      }
    } else {
      ++second_dir_;
    }
  }
}

void steps_iter::generate_ordinary() {
  assert(state == ordinary);
  assert(board_);

  if (first_dir_ != directions_end()) {
    result_ = step::validate_ordinary_step(*board_,
        elementary_step::displacement(piece_pos_, *first_dir_));
  } else {
    board_ = 0;
  }
}

void steps_iter::generate_push() {
  assert(board_);
  advance_second_to_empty();

  if (second_dir_ != directions_end()) {
    // There is a space for the second piece to be pushed to.
    result_ = step::validate_push(
      *board_,
      elementary_step::displacement(
        make_adjacent(piece_pos_, *first_dir_), *second_dir_
      ),
      elementary_step::displacement(piece_pos_, *first_dir_)
    );
  } else {
    // No more space for the second piece to be pushed to. Go to the next 
    // primary position around the piece.
    state = ordinary;
    advance_first();
    generate_ordinary_or_push();
  }
}

void steps_iter::generate_ordinary_or_push() {
  assert(board_);
  if (first_dir_ != directions_end()) {
    if (empty(make_adjacent(piece_pos_, *first_dir_), *board_)) {
      state = ordinary;
      generate_ordinary();
    } else {
      state = push;
      generate_push();
    }
  } else {
    board_ = 0;
  }
}

steps_iter steps_begin(position what, board const& board) {
  return steps_iter(what, board);
}

steps_iter steps_end() {
  return steps_iter();
}

all_steps_iter::all_steps_iter()
  : board_(0)
{ }

all_steps_iter::all_steps_iter(apns::board const& board, piece::color_t player)
  : board_(&board)
  , current_piece_(board.begin())
  , player_(player)
{
  assert(this->board_);
  forward_to_mobile();
  if (current_piece_ != board.end()) {
    current_step_ = steps_iter(current_piece_->first, board);
    forward();

  } else {
    this->board_ = 0;
  }

  assert(current_step_ != steps_end() || !this->board_);
}

void all_steps_iter::increment() {
  assert(board_);
  ++current_step_;
  forward();
  assert(current_step_ != steps_end() || !board_);
}

all_steps_iter::reference all_steps_iter::dereference() const {
  assert(board_);
  assert(current_step_ != steps_end());
  return *current_step_;
}

bool all_steps_iter::equal(all_steps_iter const& other) const {
  return
    (board_ == 0 && other.board_ == 0)
    || (board_ == other.board_
        && current_piece_ == other.current_piece_
        && current_step_ == other.current_step_
        && player_ == other.player_);
}

void all_steps_iter::forward() {
  while (current_step_ == steps_end()) {
    ++current_piece_;
    forward_to_mobile();
    if (board_ != 0) {
      current_step_ = steps_begin(current_piece_->first, *board_);
    } else {
      break;
    }
  }
}

void all_steps_iter::forward_to_mobile() {
  assert(board_);
  while (current_piece_ != board_->end() && 
         (current_piece_->second.color() != player_ || 
          frozen(current_piece_->first, *board_))) {
    ++current_piece_;
  }

  if (current_piece_ == board_->end()) {
    board_ = 0;
  }

  assert(board_ == 0 || (current_piece_->second.color() == player_ && 
                         !frozen(current_piece_->first, *board_)));
}

all_steps_iter all_steps_begin(board const& board, piece::color_t player) {
  return all_steps_iter(board, player);
}

all_steps_iter all_steps_end() {
  return all_steps_iter();
}

} // namespace apns

