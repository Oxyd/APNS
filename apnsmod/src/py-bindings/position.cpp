#include "position.hpp"
#include "piece.hpp"
#include "py-utils.hpp"

#include <boost/python.hpp>

//! Return a hash for a position.
static unsigned position_hash(apns::position const& p) {
  return
    p.column() + std::numeric_limits<apns::position::col_t>::max() + p.row();
}

void export_position() {
  using namespace boost::python;

  to_python_converter<
    std::pair<apns::position, apns::piece>,
    pair_to_tuple<apns::position, apns::piece>
  >();

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

}
