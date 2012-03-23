#include "movement.hpp"
#include "board.hpp"

#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/filter_iterator.hpp>
#include <boost/iterator/transform_iterator.hpp>

#include <boost/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/function.hpp>

#include <functional>
#include <sstream>
#include <stdexcept>
#include <cctype>

namespace {

//! Is a stronger than b? (Their colour does not matter.)
bool stronger(piece a, piece b) {
  return index_from_type(a.get_type()) < index_from_type(b.get_type());
}

/*
 * Is there a piece of the given colour adjacent to the specified position?
 *
 * \param where Neighbourhood of this position will be considered. Position referred to by where may be empty.
 * \param color What color to search for.
 * \param board Board with the pieces on it.
 */
bool colour_adjacent(position where, piece::color_t color, board const& board) {
  return std::find_if(
      adjacent_pieces_begin(board, where), adjacent_pieces_end(),
      boost::bind(&piece::get_color, _1) == color)
    != adjacent_pieces_end();
}

/**
 * Is there an friendly piece adjacent to the given one?
 *
 * \param where Where is the given piece. This position must be nonempty.
 * \param board Board with the pieces.
 */
bool friendly_adjacent(position where, board const& board) {
  piece const what = *board.get(where);  // Assuming the position is non-empty.
  return colour_adjacent(where, what.get_color(), board);
}

/**
 * Is there a stronger opponent piece adjacent to the given one?
 *
 * \param where Where is the given piece. This position must be nonempty.
 * \param board Board with the pieces.
 */
bool stronger_opponent_adjacent(position where, board const& board) {
  piece const what = *board.get(where);  // Assuming the position is non-empty.

  return std::find_if(
      adjacent_pieces_begin(board, where), adjacent_pieces_end(),
      boost::bind(&piece::get_color, _1) == opponent_color(what.get_color())
        && boost::bind(stronger, _1, what))
    != adjacent_pieces_end();
}

/**
 * Decide whether there would be a capture if a certain move were performed.
 *
 * The hypothetical move is #what moving from #old_position to #new_position. #old_position is thus assumed to be empty
 * by this function, while #new_position is assumed to contain #what. (It doesn't matter if this position actually contains
 * any other piece on the board.)
 *
 * \return Would #what be captured after such move?
 */
bool would_be_capture(piece what, position old_position, position new_position, board const& board) {
  if (trap(new_position)) {
    for (neighbourhood_iter neighbour = neighbourhood_begin(new_position); neighbour != neighbourhood_end(); ++neighbour) {
      // Is there a friendly piece adjacent?
      if (*neighbour != old_position    // Old position is assumed to be empty.
          && !empty(*neighbour, board)
          && board.get(*neighbour)->get_color() == what.get_color()) {
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
 * The hypothetical move is #what moving from position #from to position #where. The question now is whether
 * the piece standing on #pos would be captured after such move.
 *
 * \return Would piece from #pos be captured?
 */
bool would_be_capture(position pos, piece what, position from, position where, board const& board) {
  if (!empty(pos, board)
      && trap(pos)) {
    piece const& target = *board.get(pos);

    for (neighbourhood_iter neighbour = neighbourhood_begin(pos); neighbour != neighbourhood_end(); ++neighbour) {
      if (*neighbour != from
          && *neighbour != where
          && !empty(*neighbour, board)
          && board.get(*neighbour)->get_color() == target.get_color()) {
        return false;
      } else if (*neighbour == where
          && target.get_color() == what.get_color()) {
        return false;
      }
    }

    return true;
  } else {
    return false;
  }
}

typedef std::vector<elementary_step> elementary_steps_cont;

/**
 * Check for captures and insert them into a sequence of elementary steps.
 *
 * Assuming that the piece #what has moved from position #from to position #destination, this function calculates
 * all captures that have resulted from this move and inserts them into #sequence.
 *
 * \param what What piece has moved.
 * \param from From where it has moved.
 * \param destination To where it has moved.
 * \param board Contains the pieces.
 * \param sequence All detected captures will be inserted here.
 */
void check_for_captures(piece what, position from, position destination,
    board const& board, elementary_steps_cont& sequence) {
  if (would_be_capture(what, from, destination, board)) {
    elementary_step capture = elementary_step::capture(destination);
    capture.set_what(what);
    sequence.insert(sequence.end(), capture);
  } else {
    for (neighbourhood_iter neighbour = neighbourhood_begin(from); neighbour != neighbourhood_end(); ++neighbour) {
      if (would_be_capture(*neighbour, what, from, destination, board)) {
        elementary_step capture = elementary_step::capture(*neighbour);
        capture.set_what(*board.get(*neighbour));
        sequence.insert(sequence.end(), capture);
      }
    }
  }
}

//! Get the inverse direction to the given one.
direction inverse_dir(direction dir) {
  switch (dir) {
  case north:       return south;
  case east:        return west;
  case south:       return north;
  case west:        return east;
  default:          assert(!"Never gets here");
  }

  return north;
}

//! Convert a string description of an elementary step into a real elementary step, or nothing if the description isn't valid.
boost::optional<elementary_step> elementary_step_from_string(std::string const& string) {
  if (string.length() == 4) {  // Each elementary step is exactly four characters long.
    boost::optional<piece> maybe_piece = piece_from_letter(string[0]);
    position::col_t const column = string[1];
    position::row_t const row = string[2] - '0';
    char const dir_or_capture = string[3];

    if (maybe_piece
        && board::MIN_COLUMN <= column && column <= board::MAX_COLUMN
        && board::MIN_ROW <= row && row <= board::MAX_ROW) {
      piece const piece = *maybe_piece;
      position const position = ::position(row, column);

      if (dir_or_capture == 'x') {  // A capture.
        return elementary_step::capture(position, piece);
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
direction dir_from_letter(char letter) {
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

position elementary_step::get_from() const {
  return position(representation[ROW_INDEX] - '0', representation[COLUMN_INDEX]);
}

direction elementary_step::get_where() const {
  return dir_from_letter(representation[DIR_INDEX]);
}

bool elementary_step::is_capture() const {
  return representation[DIR_INDEX] == 'x';
}

boost::optional<piece> elementary_step::get_what() const {
  return piece_from_letter_unsafe(representation[PIECE_INDEX]);
}

std::string elementary_step::to_string() const {
  return std::string(representation, representation + 4);
}

void elementary_step::set_what(boost::optional<piece> new_what) {
  if (new_what) {
    representation[PIECE_INDEX] = letter_from_piece(*new_what);
  } else {
    representation[PIECE_INDEX] = ' ';
  }
}

elementary_step elementary_step::displacement(position from, direction where, boost::optional<piece> what) {
  return elementary_step(from, where, what);
}

elementary_step elementary_step::capture(position from, boost::optional<piece> what) {
  return elementary_step(from, what);
}

elementary_step::elementary_step(position from, direction where, boost::optional<piece> what) {
  representation[ROW_INDEX] = '0' + from.get_row();
  representation[COLUMN_INDEX] = from.get_column();
  representation[DIR_INDEX] = letter_from_direction(where);
  set_what(what);
}

elementary_step::elementary_step(position which, boost::optional<piece> what) {
  representation[ROW_INDEX] = '0' + which.get_row();
  representation[COLUMN_INDEX] = which.get_column();
  representation[DIR_INDEX] = 'x';
  set_what(what);
}

bool operator == (elementary_step const& lhs, elementary_step const& rhs) {
  return lhs.get_from() == rhs.get_from() && (
      (lhs.is_capture() && rhs.is_capture())
      || (!lhs.is_capture() && !rhs.is_capture() && lhs.get_where() == rhs.get_where()));
}

bool operator != (elementary_step const& lhs, elementary_step const& rhs) {
  return !operator == (lhs, rhs);
}

step_holder step::validate_ordinary_step(board const& board, elementary_step step) {
  position const& from    = step.get_from();
  direction const& where  = step.get_where();

  if (adjacent_valid(from, where)
      && !empty(from, board)
      && !frozen(from, board)
      && empty(make_adjacent(from, where), board)) {
    position const destination = make_adjacent(from, where);
    piece const& what = *board.get(from);  // Guaranteed to be non-empty by the if above.

    if (what.get_type() == piece::rabbit
        && ((what.get_color() == piece::gold && where == south)
            || (what.get_color() == piece::silver && where == north))) {
      // Rabbits can't go backwards.
      return step_holder::none;
    }

    elementary_step_seq sequence;
    step.set_what(board.get(from));
    sequence.insert(sequence.begin(), step);

    check_for_captures(what, from, destination, board, sequence);

    return step_holder(::step(sequence.begin(), sequence.end()));
  } else {
    return step_holder::none;
  }
}

step_holder step::validate_push(board const& board,
    elementary_step const& first_step, elementary_step const& second_step) {
  if (adjacent(first_step.get_from(), second_step.get_from())
      && !empty(first_step.get_from(), board)
      && !empty(second_step.get_from(), board)
      && stronger(*board.get(second_step.get_from()), *board.get(first_step.get_from()))
      && board.get(second_step.get_from())->get_color() == opponent_color(board.get(first_step.get_from())->get_color())
      && adjacent_valid(first_step.get_from(), first_step.get_where())
      && adjacent_valid(second_step.get_from(), second_step.get_where())
      && make_adjacent(second_step.get_from(), second_step.get_where()) == first_step.get_from()
      && empty(make_adjacent(first_step.get_from(), first_step.get_where()), board)
      && !frozen(second_step.get_from(), board)) {
    return make_push_pull(board, first_step, second_step);
  } else {
    return step_holder::none;
  }
}

step_holder step::validate_pull(board const& board,
    elementary_step const& first_step, elementary_step const& second_step) {
  if (adjacent(first_step.get_from(), second_step.get_from())
      && !empty(first_step.get_from(), board)
      && !empty(second_step.get_from(), board)
      && stronger(*board.get(first_step.get_from()), *board.get(second_step.get_from()))
      && board.get(first_step.get_from())->get_color() == opponent_color(board.get(second_step.get_from())->get_color())
      && adjacent_valid(first_step.get_from(), first_step.get_where())
      && adjacent_valid(second_step.get_from(), second_step.get_where())
      && make_adjacent(second_step.get_from(), second_step.get_where()) == first_step.get_from()
      && empty(make_adjacent(first_step.get_from(), first_step.get_where()), board)
      && !frozen(first_step.get_from(), board)
      ) {
    return make_push_pull(board, first_step, second_step);
  } else {
    return step_holder::none;
  }
}

step_holder step::from_string(std::string const& string) {
  // Split the input up into elementary steps separated by spaces. Convert each elementary step separately.

  std::size_t const MAX_ELEMENTARY_STEPS_POSSIBLE = 4;  // At most four elementary steps per one step are possible.

  elementary_step_seq elementary_steps;
  std::string el_step_description;
  std::istringstream input(string);
  while (std::getline(input, el_step_description, ' ')) {
    boost::optional<elementary_step> el_step = elementary_step_from_string(el_step_description);
    if (el_step) {
      elementary_steps.insert(elementary_steps.end(), *el_step);
    } else {
      return step_holder::none;
    }
  }

  if (elementary_steps.size() <= MAX_ELEMENTARY_STEPS_POSSIBLE) {
    return step_holder(step(elementary_steps.begin(), elementary_steps.end()));
  }

  return step_holder::none;
}

bool step::capture() const {
  for (el_steps_iterator el_step = step_sequence_begin(); el_step != step_sequence_end(); ++el_step)
    if (el_step->is_capture())
      return true;
  return false;
}

std::string step::to_string() const {
  return representation;
}

step::el_steps_iterator step::step_sequence_begin() const {
  return el_steps_iterator(representation.get().begin(), representation.get().end());
}

step::el_steps_iterator step::step_sequence_end() const {
  return el_steps_iterator(representation.get().end(), representation.get().end());
}

step::reverse_el_steps_iterator step::step_sequence_rbegin() const {
  return reverse_el_steps_iterator(step_sequence_end());
}

step::reverse_el_steps_iterator step::step_sequence_rend() const {
  return reverse_el_steps_iterator(step_sequence_begin());
}

std::size_t step::steps_used() const {
  return std::count_if(step_sequence_begin(), step_sequence_end(),
      !boost::bind(&elementary_step::is_capture, _1));
}

step step::make_push_pull(board const& board, elementary_step first_step, elementary_step second_step) {
  elementary_step_seq sequence;
  piece const& first_piece = *board.get(first_step.get_from());  // Assumed to be non-empty.
  first_step.set_what(first_piece);
  sequence.insert(sequence.begin(), first_step);

  position const first_destination = make_adjacent(first_step.get_from(), first_step.get_where());

  check_for_captures(first_piece, first_step.get_from(), first_destination, board, sequence);

  piece const& second_piece = *board.get(second_step.get_from());  // Assumed to be non-empty.
  second_step.set_what(second_piece);
  sequence.insert(sequence.end(), second_step);

  position const second_destination = make_adjacent(second_step.get_from(), second_step.get_where());

  check_for_captures(second_piece, second_step.get_from(), second_destination, board, sequence);

  return step(sequence.begin(), sequence.end());
}

bool operator == (step const& lhs, step const& rhs) {
  step::el_steps_iterator left = lhs.step_sequence_begin();
  step::el_steps_iterator right = rhs.step_sequence_begin();

  for (; left != lhs.step_sequence_end() && right != rhs.step_sequence_end(); ++left, ++right)
    if (*left != *right)
      return false;

  return left == lhs.step_sequence_end() && right == rhs.step_sequence_end();
}

bool operator != (step const& lhs, step const& rhs) {
  return !operator == (lhs, rhs);
}

step_holder const step_holder::none;

e_step_kind step_kind(step const& step, piece::color_t player, board const& board) {
  if (step.steps_used() == 1) {
    return ordinary;
  } else if (board.get(step.step_sequence_begin()->get_from())->get_color() == player) {
    return pull;
  } else {
    return push;
  }
}

void apply(step const& step, board& board) {
  for (::step::el_steps_iterator es = step.step_sequence_begin();
      es != step.step_sequence_end(); ++es) {
    boost::optional<piece> maybe_what = board.get(es->get_from());
    if (maybe_what) {
      piece const& what = *maybe_what;

      board.remove(es->get_from());

      if (!es->is_capture()) {
        position const new_position = make_adjacent(es->get_from(), es->get_where());
        board.put(new_position, what);
      }
    } else {
      throw std::logic_error("apply: given step doesn't correspond to given board");
    }
  }
}

void unapply(step const& step, board& board) {
  // Traverse the sequence of elementary steps *backwards*.
  for (::step::reverse_el_steps_iterator es = step.step_sequence_rbegin();
      es != step.step_sequence_rend(); ++es) {
    position const original_position = es->get_from();

    if (!es->is_capture()) {
      assert(adjacent_valid(original_position, es->get_where()));
      position const destination = make_adjacent(original_position, es->get_where());
      boost::optional<piece> maybe_what = board.get(destination);

      if (maybe_what) {
        piece const& what = *maybe_what;
        board.remove(destination);
        board.put(original_position, what);
      } else {
        throw std::logic_error("unapply: given step doesn't correspond to given board");
      }
    } else {
      assert(es->get_what());
      piece const& what = *es->get_what();
      board.put(original_position, what);
    }
  }
}

bool frozen(position position, board const& board) {
  if (!empty(position, board)) {
    return stronger_opponent_adjacent(position, board) && !friendly_adjacent(position, board);
  } else {
    throw std::logic_error("frozen: Specified position is empty");
  }
}

bool mobile(position position, board const& board) {
  return !frozen(position, board);
}

steps_iter::steps_iter()
  : board(0)
  , piece_pos(1, 'a')  // Need to init piece_pos somehow.
  , first_dir(directions_end())
  , second_dir(directions_end())
{
  assert(!board);
}

steps_iter::steps_iter(position what_piece, ::board const& board)
  : board(&board)
  , piece_pos(what_piece)
  , first_dir(directions_begin())
  , second_dir(directions_end())
  , state(ordinary)
{
  assert(this->board);

  while (first_dir != directions_end() && !adjacent_valid(piece_pos, *first_dir)) {
    ++first_dir;
  }

  generate_ordinary_or_push();

  if (this->board != 0 && !result) {
    increment();
  }

  assert(result || !this->board);
}

step steps_iter::dereference() const {
  assert(board);
  assert(result);

  return *result;
}

bool steps_iter::equal(steps_iter const& other) const {
  return
    (board == 0 && other.board == 0)
    || (board == other.board
        && piece_pos == other.piece_pos
        && first_dir == other.first_dir
        && second_dir == other.second_dir);
}

void steps_iter::increment() {
  assert(board);

  do {
    do_increment();
  } while (board != 0 && !result);

  assert(result || !board);
}

void steps_iter::do_increment() {
  assert(board);

  switch (state) {
    case ordinary: {
      // target_dir points to an empty position and we have generated the ordinary move already. Generate pulls then.
      state = pull;
    }

    // FALLTHROUGH

    case pull: {
      advance_second_to_weaker();

      if (second_dir == directions_end()) {
        // There is no weaker enemy adjacent. Let's go to the next direction.
        state = ordinary;
        advance_first();
        generate_ordinary_or_push();
      } else {
        // There is a weaker enemy adjacent, and second_dir is pointing to it. Reverse direction to
        // second_dir, then, is the direction in which it will move when pulled.
        result = step::validate_pull(*board,
            elementary_step::displacement(piece_pos, *first_dir),
            elementary_step::displacement(make_adjacent(piece_pos, *second_dir),
                inverse_dir(*second_dir)));
      }
    } break;

    case push: {
      generate_push();
    } break;
  }
}

void steps_iter::advance_first() {
  assert(board);
  ++first_dir;
  while (first_dir != directions_end() && !adjacent_valid(piece_pos, *first_dir)) {
    ++first_dir;
  }
}

void steps_iter::advance_second_to_empty() {
  assert(board);
  position const second_center = make_adjacent(piece_pos, *first_dir);

  if (second_dir == directions_end()) {
    second_dir = directions_begin();
  } else {
    ++second_dir;
  }

  while (second_dir != directions_end()
      && (!adjacent_valid(second_center, *second_dir)
          || !empty(make_adjacent(second_center, *second_dir), *board))) {
    ++second_dir;
  }
}

void steps_iter::advance_second_to_weaker() {
  assert(board);
  position const second_center = piece_pos;

  if (second_dir == directions_end()) {
    second_dir = directions_begin();
  } else {
    ++second_dir;
  }

  while (second_dir != directions_end()) {
    piece const first_piece = *board->get(piece_pos);

    if (adjacent_valid(second_center, *second_dir)
        && !empty(make_adjacent(second_center, *second_dir), *board)) {
      piece const second_piece = *board->get(make_adjacent(second_center, *second_dir));

      if (second_piece.get_color() == opponent_color(first_piece.get_color())
          && stronger(first_piece, second_piece)) {
        return;
      } else {
        ++second_dir;
      }
    } else {
      ++second_dir;
    }
  }
}

void steps_iter::generate_ordinary() {
  assert(state == ordinary);
  assert(board);

  if (first_dir != directions_end()) {
    result = step::validate_ordinary_step(*board,
        elementary_step::displacement(piece_pos, *first_dir));
  } else {
    board = 0;
  }
}

void steps_iter::generate_push() {
  assert(board);
  advance_second_to_empty();

  if (second_dir != directions_end()) {
    // There is a space for the second piece to be pushed to.
    result = step::validate_push(*board,
        elementary_step::displacement(make_adjacent(piece_pos, *first_dir), *second_dir),
        elementary_step::displacement(piece_pos, *first_dir));
  } else {
    // No more space for the second piece to be pushed to. Go to the next primary position around the piece.
    state = ordinary;
    advance_first();
    generate_ordinary_or_push();
  }
}

void steps_iter::generate_ordinary_or_push() {
  assert(board);
  if (first_dir != directions_end()) {
    if (empty(make_adjacent(piece_pos, *first_dir), *board)) {
      state = ordinary;
      generate_ordinary();
    } else {
      state = push;
      generate_push();
    }
  } else {
    board = 0;
  }
}

steps_iter steps_begin(position what, board const& board) {
  return steps_iter(what, board);
}

steps_iter steps_end() {
  return steps_iter();
}

all_steps_iter::all_steps_iter()
  : board(0)
{ }

all_steps_iter::all_steps_iter(::board const& board, piece::color_t player)
  : board(&board)
  , current_piece(board.pieces_begin())
  , player(player)
{
  assert(this->board);
  forward_to_mobile();
  if (current_piece != board.pieces_end()) {
    current_step = steps_iter(current_piece->first, board);
    forward();

  } else {
    this->board = 0;
  }

  assert(current_step != steps_end() || !this->board);
}

void all_steps_iter::increment() {
  assert(board);
  ++current_step;
  forward();
  assert(current_step != steps_end() || !board);
}

all_steps_iter::reference all_steps_iter::dereference() const {
  assert(board);
  assert(current_step != steps_end());
  return *current_step;
}

bool all_steps_iter::equal(all_steps_iter const& other) const {
  return
    (board == 0 && other.board == 0)
    || (board == other.board
        && current_piece == other.current_piece
        && current_step == other.current_step
        && player == other.player);
}

void all_steps_iter::forward() {
  while (current_step == steps_end()) {
    ++current_piece;
    forward_to_mobile();
    if (board != 0) {
      current_step = steps_begin(current_piece->first, *board);
    } else {
      break;
    }
  }
}

void all_steps_iter::forward_to_mobile() {
  assert(board);
  while (current_piece != board->pieces_end()
      && (current_piece->second.get_color() != player || frozen(current_piece->first, *board))) {
    ++current_piece;
  }

  if (current_piece == board->pieces_end()) {
    board = 0;
  }

  assert(board == 0 || (current_piece->second.get_color() == player && !frozen(current_piece->first, *board)));
}

all_steps_iter all_steps_begin(board const& board, piece::color_t player) {
  return all_steps_iter(board, player);
}

all_steps_iter all_steps_end() {
  return all_steps_iter();
}
