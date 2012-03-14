#include "board.hpp"

#include <boost/iterator/iterator_facade.hpp>
#include <boost/bind.hpp>

#include <stdexcept>
#include <cassert>
#include <utility>
#include <iterator>
#include <cctype>
#include <sstream>

#include <iostream>

namespace {

//! Convert a linear board coordinate into a pair of row, column.
std::pair<position::row_t, position::col_t> board_from_linear(std::size_t linear) {
  assert(linear < board::ROWS * board::COLUMNS);

  return std::make_pair(
      static_cast<position::row_t>(linear / board::ROWS + board::MIN_ROW),
      static_cast<position::col_t>(linear % board::COLUMNS + board::MIN_COLUMN)
  );
}

//! Convert board coordinates into linear position.
std::size_t linear_from_board(position::row_t row, position::col_t col) {
  assert(board::MIN_ROW <= row && row <= board::MAX_ROW);
  assert(board::MIN_COLUMN <= col && col <= board::MAX_COLUMN);

  return (row - board::MIN_ROW) * board::COLUMNS + (col - board::MIN_COLUMN);
}

direction const directions[] = { north, east, south, west };

}  // Anonymous namespace.

piece::piece(color_t c, type_t t)
  : color(c)
  , type(t)
{ }

piece::color_t piece::get_color() const {
  return color;
}

piece::type_t piece::get_type() const {
  return type;
}

bool operator == (piece lhs, piece rhs) {
  return lhs.get_color() == rhs.get_color()
      && lhs.get_type() == rhs.get_type();
}

bool operator != (piece lhs, piece rhs) {
  return !operator == (lhs, rhs);
}

piece::color_t opponent_color(piece::color_t player) {
  switch (player) {
  case piece::gold:   return piece::silver;
  case piece::silver: return piece::gold;
  default:            assert(!"Doesn't get here.");
  }

  return piece::silver;  // Shut up, compiler.
}

char letter_from_piece(piece p) {
  char letter = '?';
  switch (p.get_type()) {
  case piece::elephant:     letter = 'e'; break;
  case piece::camel:        letter = 'm'; break;
  case piece::horse:        letter = 'h'; break;
  case piece::dog:          letter = 'd'; break;
  case piece::cat:          letter = 'c'; break;
  case piece::rabbit:       letter = 'r'; break;
  default: assert(!"Can't get here");
  }

  if (p.get_color() == piece::gold) {
    letter = toupper(letter);
  }

  return letter;
}

boost::optional<piece> piece_from_letter(char letter) {
  piece::type_t type;
  switch (std::tolower(letter)) {
  case 'e':   type = piece::elephant;   break;
  case 'm':   type = piece::camel;      break;
  case 'h':   type = piece::horse;      break;
  case 'd':   type = piece::dog;        break;
  case 'c':   type = piece::cat;        break;
  case 'r':   type = piece::rabbit;     break;
  default:
    return boost::optional<piece>();
  }

  piece::color_t const color = (std::islower(letter) ? piece::silver : piece::gold);
  return piece(color, type);
}

char letter_from_direction(direction d) {
  switch (d) {
  case north:   return 'n';
  case east:    return 'e';
  case south:   return 's';
  case west:    return 'w';
  default: assert(!"Can't get here");
  }

  return '?';  // Shut up a compiler warning.
}

piece::color_t color_from_int(int value) {
  switch (value) {
  case piece::gold:   return piece::gold;
  case piece::silver: return piece::silver;
  default:
    throw std::domain_error("Invalid integer representation of piece::color_t was given");
  }
}

position::position(row_t row, col_t column)
  : row(row)
  , column(column)
{
  if (row < board::MIN_ROW || row > board::MAX_ROW
      || column < board::MIN_COLUMN || column > board::MAX_COLUMN) {
    std::ostringstream message;
    message << "position::position: attempted to create an invalid position: "
            << row << column;
    throw std::domain_error(message.str());
  }
}

position::row_t position::get_row() const {
  return row;
}

position::col_t position::get_column() const {
  return column;
}

position::position(row_t row, python_col_t const& col)
  : row(row)
{
  if (col.length() == 1) {
    column = static_cast<unsigned char>(col[0]);
  } else {
    throw std::runtime_error("position::position: column must be single-character string");
  }
}

position::python_col_t position::get_column_py() const {
  return std::string(1, column);
}

bool operator == (position lhs, position rhs) {
  return lhs.get_row() == rhs.get_row() && lhs.get_column() == rhs.get_column();
}

bool operator != (position lhs, position rhs) {
  return !(operator == (lhs, rhs));
}

bool operator < (position lhs, position rhs) {
  return lhs.get_row() * board::COLUMNS + lhs.get_column() - board::MIN_COLUMN
         < rhs.get_row() * board::COLUMNS + rhs.get_column() - board::MIN_COLUMN;
}

position make_adjacent(position original, direction direction) {
  int row = original.get_row();
  char column = original.get_column();

  switch (direction) {
    case north:   ++row;    break;
    case south:   --row;    break;
    case east:    ++column; break;
    case west:    --column; break;
    default:      assert(!"Can't reach this.");
  }

  return position(row, column);  // Will throw if the position is invalid.
}

bool adjacent_valid(position pos, direction direction) {
  switch (direction) {
    case north:   return pos.get_row() < board::MAX_ROW;
    case south:   return pos.get_row() > board::MIN_ROW;
    case east:    return pos.get_column() < board::MAX_COLUMN;
    case west:    return pos.get_column() > board::MIN_COLUMN;
    default:      assert(!"Can't reach this."); return false;
  }
}

bool adjacent(position first, position second) {
  return
    (
        (
               (first.get_column() == second.get_column() + 1)
            || (first.get_column() == second.get_column() - 1)
        )
        && (
            first.get_row() == second.get_row()
        )
    )
    || (
        (
               (first.get_row() == second.get_row() + 1)
            || (first.get_row() == second.get_row() - 1)
        ) && (
            first.get_column() == second.get_column()
        )
    );

  // Well, yes. This is fairly ugly.
}

piece::type_t type_from_int(int value) {
  switch (value) {
  case piece::elephant:   return piece::elephant;
  case piece::camel:      return piece::camel;
  case piece::horse:      return piece::horse;
  case piece::dog:        return piece::dog;
  case piece::cat:        return piece::dog;
  case piece::rabbit:     return piece::rabbit;
  default:
    throw std::domain_error("Invalid integer representation of piece::type_t was given");
  }
}

board::pieces_iterator::pieces_iterator()
  : board::pieces_iterator::iterator_adaptor_(base_type())
  , pos(0)
{ }

board::pieces_iterator::pieces_iterator(base_type original, std::size_t pos)
  : board::pieces_iterator::iterator_adaptor_(original)
  , pos(pos)
{
  forward_to_nonempty();
}

board::pieces_iterator::reference board::pieces_iterator::dereference() const {
  assert(pos < board::ROWS * board::COLUMNS);
  assert(*base());

  std::pair<position::row_t, position::col_t> const coordinates = board_from_linear(pos);
  return std::make_pair(position(coordinates.first, coordinates.second), **base());
}

void board::pieces_iterator::increment() {
  // Keep incrementing the iterator until we find a nonempty position. If there is no more nonempty position on the board,
  // let the base iterator go to the one-past-the-end position and let pos = one-past-the-end-index.

  ++pos;
  ++base_reference();

  forward_to_nonempty();
}

void board::pieces_iterator::decrement() {
  // Iverse algorithm to increment(). Keep decrementing until we reach either a nonempty position or until we reach the
  // (1, 'a') coordinate.

  --pos;
  --base_reference();

  reverse_to_nonempty();
}

void board::pieces_iterator::forward_to_nonempty() {
  while (pos < board::ROWS * board::COLUMNS) {
    if (*base()) {
      return;  // We're done, this is the next nonempty position.
    }

    ++pos;
    ++base_reference();
  }

  // There are no more nonempty positions. Furthermore, base and pos contain their one-past-the-end values.
}

void board::pieces_iterator::reverse_to_nonempty() {
  // Once it gets down to pos == 0, then we don't have to do anything more: either there's really something here or we got
  // here by decrementing a begin() iterator which results in undefined behaviour.
  while (pos > 0) {
    if (*base()) {
      return;
    }

    --pos;
    --base_reference();
  }
}

void board::put(position where, piece what) {
  std::size_t const linear = linear_from_board(where.get_row(), where.get_column());
  if (!pieces[linear]) {
    pieces[linear] = what;
  } else {
    throw std::logic_error("board: Attempted to put a piece at an occupied position");
  }
}

void board::remove(position where) {
  std::size_t const linear = linear_from_board(where.get_row(), where.get_column());
  if (pieces[linear]) {
    pieces[linear] = boost::optional<piece>();
  } else {
    throw std::logic_error("board: Attempted to remove a piece from a vacant position");
  }
}

boost::optional<piece> board::get(position from) const {
  return pieces[linear_from_board(from.get_row(), from.get_column())];
}

board::pieces_iterator board::pieces_begin() const {
  pieces_iterator it(pieces.begin(), 0);
  return it;
}

board::pieces_iterator board::pieces_end() const {
  pieces_iterator it(pieces.end(), ROWS * COLUMNS);
  return it;
}

bool operator == (board const& lhs, board const& rhs) {
  // This implementation assumes that the traversal order of pieces iterators is always the same for two boards with same
  // contents.

  board::pieces_iterator left = lhs.pieces_begin();
  board::pieces_iterator right = rhs.pieces_begin();

  for (; left != lhs.pieces_end() && right != rhs.pieces_end(); ++left, ++right) {
    if (left->first != right->first || left->second != right->second) {
      return false;
    }
  }

  return left == lhs.pieces_end() && right == rhs.pieces_end();
}

bool operator != (board const& lhs, board const& rhs) {
  return !operator == (lhs, rhs);
}

std::string string_from_board(board const& board) {
  std::ostringstream output;

  for (board::pieces_iterator pos_piece = board.pieces_begin(); pos_piece != board.pieces_end(); ++pos_piece) {
    if (pos_piece != board.pieces_begin()) {
      output << ' ';
    }

    position const& pos = pos_piece->first;
    piece const& piece = pos_piece->second;

    output << pos.get_row() << pos.get_column() << letter_from_piece(piece);
  }

  return output.str();
}

bool empty(position pos, board const& board) {
  return board.get(pos).get_ptr() == 0;
}

bool trap(position pos) {
  return (pos.get_column() == 'c' && (pos.get_row() == 3 || pos.get_row() == 6))
         || (pos.get_column() == 'f' && (pos.get_row() == 3 || pos.get_row() == 6));
}

directions_iter directions_begin() {
  return directions;
}

directions_iter directions_end() {
  return directions + 4;
}

neighbourhood_iter::neighbourhood_iter()
  : direction(directions_end())
  , center(1, 'a')  // Need to initialise this somehow
{ }

neighbourhood_iter::neighbourhood_iter(position center)
  : direction(directions_begin())
  , center(center)
{
  forward_to_valid();
}

void neighbourhood_iter::increment() {
  ++direction;
  forward_to_valid();
}

neighbourhood_iter::reference neighbourhood_iter::dereference() const {
  return make_adjacent(center, *direction);
}

bool neighbourhood_iter::equal(neighbourhood_iter const& other) const {
  return
    (direction == directions_end() && other.direction == directions_end())
    || (direction == other.direction
        && center == other.center);
}

void neighbourhood_iter::forward_to_valid() {
  while (direction != directions_end() && !adjacent_valid(center, *direction)) {
    ++direction;
  }
}

neighbourhood_iter neighbourhood_begin(position center) {
  return neighbourhood_iter(center);
}

neighbourhood_iter neighbourhood_end() {
  return neighbourhood_iter();
}

adjacent_pieces_iter::adjacent_pieces_iter()
  : board(0)
{ }

adjacent_pieces_iter::adjacent_pieces_iter(position center, ::board const& board)
  : iterator_adaptor_(center)
  , board(&board)
{
  forward_to_nonempty();
}

void adjacent_pieces_iter::increment() {
  ++base_reference();
  forward_to_nonempty();
}

adjacent_pieces_iter::reference adjacent_pieces_iter::dereference() const {
  return *board->get(*base());
}

bool adjacent_pieces_iter::equal(adjacent_pieces_iter const& other) const {
  return
    (board == 0 && other.board == 0)
    || (
        board == other.board
        && base_reference() == other.base_reference());
}

void adjacent_pieces_iter::forward_to_nonempty() {
  while (base_reference() != base_type() && empty(*base(), *board)) {
    ++base_reference();
  }

  if (base_reference() == base_type()) {
    board = 0;
  }
}

adjacent_pieces_iter adjacent_pieces_begin(board const& board, position center) {
  return adjacent_pieces_iter(center, board);
}

adjacent_pieces_iter adjacent_pieces_end() {
  return adjacent_pieces_iter();
}
