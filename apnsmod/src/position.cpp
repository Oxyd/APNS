#include "position.hpp"
#include "board.hpp"

#include <stdexcept>
#include <sstream>
#include <string>
#include <cassert>

namespace apns {

namespace {

direction const directions[] = {
  north, east, south, west
};

} // anonymous namespace

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

position make_adjacent(position original, direction direction) {
  int row = original.row();
  char column = original.column();

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

// MSVS doesn't seem to like this.
#ifndef _MSC_VER
position::row_t const position::MIN_ROW;
position::row_t const position::MAX_ROW;
position::col_t const position::MIN_COLUMN;
position::col_t const position::MAX_COLUMN;

position::row_t const board::ROWS;
position::col_t const board::COLUMNS;

#endif

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

} // namespace apns
