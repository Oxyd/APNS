#include "board.hpp"

#include "py-utils.hpp"

#include <boost/python.hpp>

#include <limits>

namespace {

/**
 * Macro to ease generation of a bunch of helper functions. These are needed to provide a Python getter for
 * the C++ static member constants of class board. Exporting these constants directly seems to cause linker problems with
 * MSVC.
 *
 * XXX: Not anymore I guess.
 */
#define MAKE_GETTER(type, name, constant) \
  position:: type board_ ## name () {     \
    return board:: constant;              \
  }

MAKE_GETTER(row_t, rows, ROWS)
MAKE_GETTER(col_t, columns, COLUMNS)
MAKE_GETTER(row_t, min_row, MIN_ROW)
MAKE_GETTER(row_t, max_row, MAX_ROW)
MAKE_GETTER(col_t, min_column, MIN_COLUMN)
MAKE_GETTER(col_t, max_column, MAX_COLUMN)

//! Make a copy of a board instance. This is so that Python can explicitely ask for copies of board objects.
board board_copy(board const& original) {
  return original;
}

//! Return a hash for a position.
unsigned position_hash(position const& p) {
  return p.get_column() + std::numeric_limits<position::col_t>::max() + p.get_row();
}

boost::optional<piece> py_piece_from_letter(std::string const& letter) {
  if (letter.length() == 1)
    return piece_from_letter(letter[0]);
  else
    throw std::logic_error("Expected one-letter string");
}

std::string py_letter_from_piece(piece const& p) {
  return std::string(1, letter_from_piece(p));
}

} // anonymous namespace

/**
 * Export types and functions declared in board.hpp.
 */
void export_board() {
  using namespace boost::python;

  to_python_converter<boost::optional<piece>, optional_to_T<piece> >();
  to_python_converter<std::pair<position, piece>, pair_to_tuple<position, piece> >();

  {
    scope piece_scope =
        class_<piece>("Piece",
          "Represents a single Arimaa game piece",
          init<piece::color_t, piece::type_t>())
          .add_property("color", &piece::get_color)
          .add_property("type", &piece::get_type)
          ;

    enum_<piece::color_t>("Color",
      "Color of a piece. Either gold or silver.")
      .value("gold", piece::gold)
      .value("silver", piece::silver)
      ;

    enum_<piece::type_t>("Type",
      "Type of the piece. In Arimaa, there are six types of pieces: elephant, camel, horse, dog, cat, and rabbit")
      .value("elephant", piece::elephant)
      .value("camel", piece::camel)
      .value("horse", piece::horse)
      .value("dog", piece::dog)
      .value("cat", piece::cat)
      .value("rabbit", piece::rabbit)
      ;
  }

  def("pieceFromLetter", &py_piece_from_letter,
      "pieceFromLetter(letter) -> Piece\n\nConstruct a piece from its Arimaa letter.");
  def("letterFromPiece", &py_letter_from_piece,
      "letterFromPiece(Piece) -> str\n\nGet the letter corresponding a to a piece.");

  enum_<direction>("Direction",
    "The four cardinal directions")
    .value("north", north)
    .value("south", south)
    .value("east", east)
    .value("west", west)
    ;

  class_<position>("Position",
    "A position on the board. Position is immutable.",
    init<position::row_t, position::python_col_t>())
    .add_property("row", &position::get_row)
    .add_property("column", &position::get_column_py)

    .def(self == self)
    .def(self != self)
    .def(self < self)

    .def("__hash__", &position_hash)
    ;

  def("makeAdjacent", &make_adjacent,
      "makeAdjacent(Position, Direction) -> Position\n\n"
      "Get a position adjacent in the given direction to the original position. If this would result in an invalid"
      "position, throw RuntimeError.");

  def("adjacentValid", &adjacent_valid,
      "adjacentValid(Position, Direction) -> bool\n\n"
      "Is a position adjacent to the given one valid? That is, is it inside the board?");

  def("adjacent", &adjacent,
      "adjacent(Position, Position) -> bool\n\n"
      "Are two given positions adjacent to each other?");

  class_<board>("Board", "Game board with pieces on it.")
    .add_static_property("ROWS", &board_rows, "Number of rows on the board.")
    .add_static_property("COLUMNS", &board_columns, "Number of columns on the board.")
    .add_static_property("MIN_COLUMN", &board_min_column, "The lowest coordinate of a row.")
    .add_static_property("MAX_COLUMN", &board_max_column, "The highest coordinate of a row.")
    .add_static_property("MIN_ROW", &board_min_row, "The lowest coordinate of a row.")
    .add_static_property("MAX_ROW", &board_max_row, "The highest coordinate of a row.")

    .def("put", &board::put, "b.put(Position, Piece) -> None")
    .def("remove", &board::remove, "b.remove(Position) -> None")
    .def("get", &board::get,
        "b.get(Position) -> Piece\n\n"
        "Retreive the piece stored at the given position. If there is no piece stored,"
        "return None.")

    .def("copy", &board_copy,
        "b.copy() -> Board\n\n",
        return_value_policy<return_by_value>(),
        "Make a copy of this Board")

    .add_property("pieces", range(&board::pieces_begin, &board::pieces_end))
    ;

  def("empty", &empty,
      "empty(Position, Board) -> bool\n\n"
      "Is the given position on the given board empty?");

  def("trap", &trap,
      "trap(Position) -> bool\n\n"
      "Is the given position a trap?");
}

