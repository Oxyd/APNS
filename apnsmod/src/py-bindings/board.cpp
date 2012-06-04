#include "board.hpp"
#include "py-utils.hpp"

#include <boost/python.hpp>
#include <limits>

namespace {

//! Make a copy of a board instance. This is so that Python can explicitely ask
//! for copies of board objects.
apns::board board_copy(apns::board const& original) {
  return original;
}

//! Return a hash for a position.
unsigned position_hash(apns::position const& p) {
  return p.column() + std::numeric_limits<apns::position::col_t>::max() + p.row();
}

boost::optional<apns::piece> py_piece_from_letter(std::string const& letter) {
  if (letter.length() == 1)
    return apns::piece_from_letter(letter[0]);
  else
    throw std::logic_error("Expected one-letter string");
}

std::string py_letter_from_piece(apns::piece const& p) {
  return std::string(1, apns::letter_from_piece(p));
}

} // anonymous namespace

/**
 * Export types and functions declared in board.hpp.
 */
void export_board() {
  using namespace boost::python;

  to_python_converter<boost::optional<apns::piece>, optional_to_T<apns::piece> >();
  to_python_converter<std::pair<apns::position, apns::piece>, pair_to_tuple<apns::position, apns::piece> >();

  {
    scope piece_scope =
        class_<apns::piece>("Piece",
          "Represents a single Arimaa game piece",
          init<apns::piece::color_t, apns::piece::type_t>())
          .add_property("color", &apns::piece::color)
          .add_property("type", &apns::piece::type)
          ;

    enum_<apns::piece::color_t>("Color",
      "Color of a piece. Either gold or silver.")
      .value("gold", apns::piece::gold)
      .value("silver", apns::piece::silver)
      ;

    enum_<apns::piece::type_t>("Type",
      "Type of the piece. In Arimaa, there are six types of pieces: elephant, camel, horse, dog, cat, and rabbit")
      .value("elephant", apns::piece::elephant)
      .value("camel", apns::piece::camel)
      .value("horse", apns::piece::horse)
      .value("dog", apns::piece::dog)
      .value("cat", apns::piece::cat)
      .value("rabbit", apns::piece::rabbit)
      ;
  }

  def("pieceFromLetter", &py_piece_from_letter,
      "pieceFromLetter(letter) -> Piece\n\nConstruct a piece from its Arimaa letter.");
  def("letterFromPiece", &py_letter_from_piece,
      "letterFromPiece(Piece) -> str\n\nGet the letter corresponding a to a piece.");

  enum_<apns::direction>("Direction",
    "The four cardinal directions")
    .value("north", apns::north)
    .value("south", apns::south)
    .value("east", apns::east)
    .value("west", apns::west)
    ;

  class_<apns::position>("Position",
    "A position on the board. Position is immutable.",
    init<apns::position::row_t, std::string const&>())
    .def_readonly("MIN_COLUMN",
                  &apns::position::MIN_COLUMN,
                  "The lowest coordinate of a column.")
    .def_readonly("MAX_COLUMN",
                  &apns::position::MAX_COLUMN,
                  "The highest coordinate of a column.")
    .def_readonly("MIN_ROW",
                  &apns::position::MIN_ROW,
                  "The lowest coordinate of a row.")
    .def_readonly("MAX_ROW",
                  &apns::position::MAX_ROW,
                  "The highest coordinate of a row.")

    .add_property("row", &apns::position::row)
    .add_property("column", &apns::position::py_column)

    .def(self == self)
    .def(self != self)
    .def(self < self)

    .def("__hash__", &position_hash)
    ;

  def("makeAdjacent", &apns::make_adjacent,
      "makeAdjacent(Position, Direction) -> Position\n\n"
      "Get a position adjacent in the given direction to the original position. If this would result in an invalid"
      "position, throw RuntimeError.");

  def("adjacentValid", &apns::adjacent_valid,
      "adjacentValid(Position, Direction) -> bool\n\n"
      "Is a position adjacent to the given one valid? That is, is it inside the board?");

  def("adjacent", &apns::adjacent,
      "adjacent(Position, Position) -> bool\n\n"
      "Are two given positions adjacent to each other?");

  class_<apns::board>("Board", "Game board with pieces on it.")
    .def_readonly("ROWS",
                  &apns::board::ROWS,
                  "Number of rows on the board.")
    .def_readonly("COLUMNS",
                  &apns::board::COLUMNS,
                  "Number of columns on the board.")

    .def("put", &apns::board::put, "b.put(Position, Piece) -> None")
    .def("remove", &apns::board::remove, "b.remove(Position) -> None")
    .def("get", &apns::board::get,
        "b.get(Position) -> Piece\n\n"
        "Retreive the piece stored at the given position. If there is no piece stored,"
        "return None.")

    .def("copy", &board_copy,
        "b.copy() -> Board\n\n",
        return_value_policy<return_by_value>(),
        "Make a copy of this Board")

    .add_property("pieces", range(&apns::board::begin, &apns::board::end))

    .def(self == self)
    .def(self != self)
    ;

  def("empty", &apns::empty,
      "empty(Position, Board) -> bool\n\n"
      "Is the given position on the given board empty?");

  def("trap", &apns::trap,
      "trap(Position) -> bool\n\n"
      "Is the given position a trap?");
}

