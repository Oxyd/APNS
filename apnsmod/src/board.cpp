#include "board.hpp"

#include <boost/lexical_cast.hpp>
#include <boost/bind.hpp>

#include <stdexcept>
#include <cassert>
#include <utility>
#include <cctype>
#include <sstream>


namespace apns {

namespace {

//! Convert a linear board coordinate into a pair of row, column.
std::pair<position::row_t, position::col_t>
board_from_linear(std::size_t linear) {
  assert(linear < board::ROWS * board::COLUMNS);

  return std::make_pair(
      static_cast<position::row_t>(linear / board::ROWS + position::MIN_ROW),
      static_cast<position::col_t>(linear % board::COLUMNS + position::MIN_COLUMN)
  );
}

//! Convert board coordinates into linear position.
std::size_t linear_from_board(position::row_t row, position::col_t col) {
  assert(position::MIN_ROW <= row && row <= position::MAX_ROW);
  assert(position::MIN_COLUMN <= col && col <= position::MAX_COLUMN);

  return (row - position::MIN_ROW) * board::COLUMNS + (col - position::MIN_COLUMN);
}

direction const directions[] = {
  north, east, south, west
};

char letter_from_pair(piece::color_t c, piece::type_t t) {
  char letter = t;
  if (c == piece::gold)
    letter &= ~0x20;  // Assuming ASCII here. Convert to uppercase.

  return letter;
}

// Index table for the de Bruijn multiplication algorithm below.
std::size_t const index64[64] = {
   63,  0, 58,  1, 59, 47, 53,  2,
   60, 39, 48, 27, 54, 33, 42,  3,
   61, 51, 37, 40, 49, 18, 28, 20,
   55, 30, 34, 11, 43, 14, 22,  4,
   62, 57, 46, 52, 38, 26, 32, 41,
   50, 36, 17, 19, 29, 10, 13, 21,
   56, 45, 25, 31, 35, 16,  9, 12,
   44, 24, 15,  8, 23,  7,  6,  5
};

std::size_t bitscan(boost::uint64_t mask) {
  // NB: I split this into two constants OR'd together so that there is no
  // problem with 64-bit integer constants on 32-bit systems.
  boost::uint64_t const DE_BRUIJN =
    (boost::uint64_t(0x07EDD5E5ul) << 32) |
     boost::uint64_t(0x9A4E28C2ul);

  assert(mask != 0);
  return index64[((mask & -mask) * DE_BRUIJN) >> 58];
}

}  // Anonymous namespace.

piece::piece(color_t c, type_t t) : 
  data_(letter_from_pair(c, t))
{
  assert(data_ == 'e' || data_ == 'm' || data_ == 'h' || data_ == 'd' ||
         data_ == 'c' || data_ == 'r' || data_ == 'E' || data_ == 'M' ||
         data_ == 'H' || data_ == 'D' || data_ == 'C' || data_ == 'R');
}

piece::color_t piece::color() const {
  assert(data_ == 'e' || data_ == 'm' || data_ == 'h' || data_ == 'd' ||
         data_ == 'c' || data_ == 'r' || data_ == 'E' || data_ == 'M' ||
         data_ == 'H' || data_ == 'D' || data_ == 'C' || data_ == 'R');
  return data_ & 0x20 ? piece::silver : piece::gold;
}

piece::type_t piece::type() const {
  assert(data_ == 'e' || data_ == 'm' || data_ == 'h' || data_ == 'd' ||
         data_ == 'c' || data_ == 'r' || data_ == 'E' || data_ == 'M' ||
         data_ == 'H' || data_ == 'D' || data_ == 'C' || data_ == 'R');
  return static_cast<piece::type_t>(data_ | 0x20);
}

bool piece::equal(piece const& other) const {
  return data_ == other.data_;
}

boost::array<piece::color_t const, 2> const COLORS = { {
  piece::gold, piece::silver
} };
boost::array<piece::type_t const, 6> const TYPES = { {
  piece::elephant, piece::camel, piece::horse, piece::dog, piece::cat,
  piece::rabbit
} };

std::size_t index_from_color(piece::color_t color) {
  if (color == piece::gold)
    return 0;
  else
    return 1;
}

std::size_t index_from_type(piece::type_t type) {
  switch (type) {
  case piece::elephant:   return 0;
  case piece::camel:      return 1;
  case piece::horse:      return 2;
  case piece::dog:        return 3;
  case piece::cat:        return 4;
  case piece::rabbit:     return 5;
  default:                assert(!"Can't happen"); return 0;
  }
}

bool operator == (piece lhs, piece rhs) {
  return lhs.equal(rhs);
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
  return p.letter();
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
    throw std::domain_error(
      "Invalid integer representation of piece::color_t was given"
    );
  }
}

position::position(row_t row, std::string const& col) {
  if (col.length() != 1)
    throw std::invalid_argument(
      "position::position: Expected a single-character string"
    );

  col_t column = col[0];

  if (row < MIN_ROW || row > MAX_ROW ||
      column < MIN_COLUMN || column > MAX_COLUMN) {
    std::ostringstream message;
    message << "position::position: attempted to create an invalid position: "
            << row << column;
    throw std::invalid_argument(message.str());
  }

  data_ = ((row - MIN_ROW) << ROW_OFFSET) | (column - MIN_COLUMN);
}

position::row_t position::row() const {
  return (data_ >> ROW_OFFSET) + MIN_ROW;
}

position::col_t position::column() const {
  return (data_ & COLUMN_MASK) + MIN_COLUMN;
}

std::string position::py_column() const {
  return std::string(1, column());
}

void position::set_row(row_t new_row) {
  if (new_row >= MIN_ROW && new_row <= MAX_ROW)
    data_ = ((new_row - MIN_ROW) << ROW_OFFSET) | (data_ & COLUMN_MASK);
  else
    throw std::invalid_argument("position::set_row");
}

void position::set_column(col_t new_column) {
  if (new_column >= MIN_COLUMN && new_column <= MAX_COLUMN)
    data_ = (new_column - MIN_COLUMN) | (data_ & ~COLUMN_MASK);
  else
    throw std::invalid_argument("position::set_column");
}

position& position::operator += (std::size_t n) {
  data_ += n;
  if (data_ > MAX || n > MAX)
    throw std::invalid_argument("position::operator +=");

  return *this;
}

position& position::operator -= (std::size_t n) {
  data_ -= n;

  // data_ is unsigned, so it'll wrap around if someone tries to decrement too
  // much.
  if (data_ > MAX || n > MAX)
    throw std::invalid_argument("position::operator -=");

  return *this;
}

bool adjacent_valid(position pos, direction direction) {
  switch (direction) {
    case north:   return pos.row() < position::MAX_ROW;
    case south:   return pos.row() > position::MIN_ROW;
    case east:    return pos.column() < position::MAX_COLUMN;
    case west:    return pos.column() > position::MIN_COLUMN;
    default:      assert(!"Can't reach this."); return false;
  }
}

bool adjacent(position first, position second) {
  return
    (
        (
               (first.column() == second.column() + 1)
            || (first.column() == second.column() - 1)
        )
        && (
            first.row() == second.row()
        )
    )
    || (
        (
               (first.row() == second.row() + 1)
            || (first.row() == second.row() - 1)
        ) && (
            first.column() == second.column()
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
    throw std::domain_error(
      "Invalid integer representation of piece::type_t was given"
    );
  }
}

// MSVS doesn't seem to like this.
#ifndef _MSC_VER
position::row_t const position::MIN_ROW;
position::row_t const position::MAX_ROW;
position::col_t const position::MIN_COLUMN;
position::col_t const position::MAX_COLUMN;

position::row_t const board::ROWS;
position::col_t const board::COLUMNS;

#endif

board::mask::iterator::iterator(mask::bits_t m)
  : mask_(m)
  , pos_(position(position::MIN_ROW, position::MIN_COLUMN)) {
  if (mask_ > 0)
    offset_ = bitscan(mask_);
}

void board::mask::iterator::increment() {
  pos_ += offset_ + 1;
  mask_ >>= offset_ + 1;
  if (mask_ > 0)
    offset_ = bitscan(mask_);
}

namespace {

// NB: These exist mainly for the purpose of promoting stuff to uint64_t in
// integral expressions.

boost::uint64_t const U1 = 1;
boost::uint64_t const ROW1 = 0xFF;
boost::uint64_t const COL1 =
  (U1 << (0 * board::COLUMNS)) |
  (U1 << (1 * board::COLUMNS)) |
  (U1 << (2 * board::COLUMNS)) |
  (U1 << (3 * board::COLUMNS)) |
  (U1 << (4 * board::COLUMNS)) |
  (U1 << (5 * board::COLUMNS)) |
  (U1 << (6 * board::COLUMNS)) |
  (U1 << (7 * board::COLUMNS))
  ;

}

board::mask const board::mask::ROW[] = {
  mask(ROW1 << (0 * board::COLUMNS)),
  mask(ROW1 << (1 * board::COLUMNS)),
  mask(ROW1 << (2 * board::COLUMNS)),
  mask(ROW1 << (3 * board::COLUMNS)),
  mask(ROW1 << (4 * board::COLUMNS)),
  mask(ROW1 << (5 * board::COLUMNS)),
  mask(ROW1 << (6 * board::COLUMNS)),
  mask(ROW1 << (7 * board::COLUMNS))
};

board::mask const board::mask::COLUMN[] = {
  mask(COL1 << 0),
  mask(COL1 << 1),
  mask(COL1 << 2),
  mask(COL1 << 3),
  mask(COL1 << 4),
  mask(COL1 << 5),
  mask(COL1 << 6),
  mask(COL1 << 7)
};

board::mask const board::mask::TRAPS =
  (board::mask::row(3) & board::mask::column('c')) |
  (board::mask::row(3) & board::mask::column('f')) |
  (board::mask::row(6) & board::mask::column('c')) |
  (board::mask::row(6) & board::mask::column('f'));

position board::mask::first_set() const {
  if (bits_)
    return position(position::MIN_ROW, position::MIN_COLUMN) + bitscan(bits_);
  else
    throw std::logic_error("board::mask::first_set");
}

board::mask board::mask::shift(direction dir) const {
  switch (dir) {
  case north: return mask(bits_ << COLUMNS);
  case south: return mask(bits_ >> COLUMNS);
  case east:  return mask((bits_ << 1) & ~column(position::MIN_COLUMN).bits_);
  case west:  return mask((bits_ >> 1) & ~column(position::MAX_COLUMN).bits_);
  default:    assert(!"Can't get here"); return board::mask();
  }
}

board::iterator::iterator(board const& board, mask::const_iterator b)
  : iterator::iterator_adaptor_(b)
  , board_(&board) { }

std::pair<position, piece> board::iterator::dereference() const {
  assert(board_);
  assert(base() != board::mask::iterator());

  position const pos = *base();
  piece const    piece = *board_->get(pos);

  return std::make_pair(pos, piece);
}

void board::put(position where, piece what) {
  if (!occupied_[where]) {
    occupied_[where] = true;
    players_[index_from_color(what.color())][where] = true;
    types_[index_from_type(what.type())][where] = true;
  } else throw std::logic_error("board::put: Position is occupied");
}

void board::remove(position from) {
  if (occupied_[from]) {
    occupied_[from ] = false;
    for (players_masks::iterator p = players_.begin(); p != players_.end(); ++p)
      p->set(from, false);
    for (types_masks::iterator t = types_.begin(); t != types_.end(); ++t)
      t->set(from, false);
  } else throw std::logic_error("board::remove: Position is empty");
}

void board::clear() {
  occupied_ = mask();
  for (players_masks::iterator p = players_.begin(); p != players_.end(); ++p)
    *p = mask();
  for (types_masks::iterator t = types_.begin(); t != types_.end(); ++t)
    *t = mask();
}

boost::optional<piece> board::get(position from) const {
  if (occupied_[from]) {
    piece::color_t color;
    piece::type_t  type;

    for (players_masks::const_iterator p = players_.begin();
         p != players_.end(); ++p)
      if (p->get(from)) { color = COLORS[p - players_.begin()]; break; }
    for (types_masks::const_iterator t = types_.begin();
         t != types_.end(); ++t)
      if (t->get(from)) { type = TYPES[t - types_.begin()]; break; }

    return piece(color, type);
  } else return boost::none;
}

bool board::equal(board const& other) const {
  return
    occupied_ == other.occupied_ &&
    players_ == other.players_ &&
    types_ == other.types_;
}

bool board::less(board const& other) const {
  return
    occupied_ < other.occupied_ ||
    (occupied_ == other.occupied_ &&
      (players_ < other.players_ ||
        (players_ == other.players_ &&
          types_ < other.types_)));
}

board::iterator board::begin() const {
  return iterator(*this, occupied_.begin());
}

board::iterator board::end() const {
  return iterator(*this, occupied_.end());
}

std::string string_from_board(board const& board) {
  std::ostringstream output;

  for (board::iterator pos_piece = board.begin();
       pos_piece != board.end(); ++pos_piece) {
    if (pos_piece != board.begin())
      output << ' ';

    position const pos  = pos_piece->first;
    piece const piece   = pos_piece->second;

    output << static_cast<unsigned>(pos.row())
           << static_cast<char>(pos.column())
           << letter_from_piece(piece);
  }

  return output.str();
}

void board_from_string(std::string const& string, board& board) {
  board.clear();

  std::istringstream input(string);
  std::string piece_and_position;
  while (std::getline(input, piece_and_position, ' ')) {
    if (piece_and_position.length() != 3)
      throw std::runtime_error("Invalid board string");

    unsigned const row = boost::lexical_cast<unsigned>(piece_and_position[0]);
    char const     col = boost::lexical_cast<char>(piece_and_position[1]);

    boost::optional<piece> maybe_piece =
      piece_from_letter(piece_and_position[2]);
    if (maybe_piece)
      board.put(position(row, col), *maybe_piece);
    else
      throw std::runtime_error("Invalid board string");
  }
}

bool empty(position pos, board const& board) {
  return board.get(pos).get_ptr() == 0;
}

bool trap(position pos) {
  return board::mask::TRAPS[pos];
}

board_masks masks_from_board(board const& board) {
  board_masks result;
  for (board::iterator it = board.begin(); it != board.end(); ++it) {
    result.occupied[it->first] = true;
    result.players[index_from_color(it->second.color())][it->first] = true;
    result.types[index_from_type(it->second.type())][it->first] = true;
  }

  return result;
}

board::mask neighbourhood(position pos) {
  board::mask center;
  center[pos] = true;
  return
    center.shift(north) |
    center.shift(south) |
    center.shift(east)  |
    center.shift(west)
    ;
}

directions_iter directions_begin() {
  return directions;
}

directions_iter directions_end() {
  return directions + 4;
}

neighbourhood_iter::neighbourhood_iter()
  : direction_(directions_end())
  , center_(1, 'a')  // Need to initialise this somehow
{ }

neighbourhood_iter::neighbourhood_iter(position center)
  : direction_(directions_begin())
  , center_(center)
{
  forward_to_valid();
}

void neighbourhood_iter::increment() {
  ++direction_;
  forward_to_valid();
}

neighbourhood_iter::reference neighbourhood_iter::dereference() const {
  return make_adjacent(center_, *direction_);
}

bool neighbourhood_iter::equal(neighbourhood_iter const& other) const {
  return
    (direction_ == directions_end() && other.direction_ == directions_end())
    || (direction_ == other.direction_
        && center_ == other.center_);
}

void neighbourhood_iter::forward_to_valid() {
  while (direction_ != directions_end() &&
         !adjacent_valid(center_, *direction_)) {
    ++direction_;
  }
}

neighbourhood_iter neighbourhood_begin(position center) {
  return neighbourhood_iter(center);
}

neighbourhood_iter neighbourhood_end() {
  return neighbourhood_iter();
}

adjacent_pieces_iter::adjacent_pieces_iter()
  : board_(0)
{ }

adjacent_pieces_iter::adjacent_pieces_iter(position center,
                                           apns::board const& board)
  : iterator_adaptor_(center)
  , board_(&board)
{
  forward_to_nonempty();
}

void adjacent_pieces_iter::increment() {
  ++base_reference();
  forward_to_nonempty();
}

adjacent_pieces_iter::reference adjacent_pieces_iter::dereference() const {
  return *board_->get(*base());
}

bool adjacent_pieces_iter::equal(adjacent_pieces_iter const& other) const {
  return
    (board_ == 0 && other.board_ == 0)
    || (
        board_ == other.board_
        && base_reference() == other.base_reference());
}

void adjacent_pieces_iter::forward_to_nonempty() {
  while (base_reference() != base_type() && empty(*base(), *board_)) {
    ++base_reference();
  }

  if (base_reference() == base_type()) {
    board_ = 0;
  }
}

adjacent_pieces_iter adjacent_pieces_begin(board const& board, position center) 
{
  return adjacent_pieces_iter(center, board);
}

adjacent_pieces_iter adjacent_pieces_end() {
  return adjacent_pieces_iter();
}

} // namespace apns

