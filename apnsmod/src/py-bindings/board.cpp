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

} // anonymous namespace

/**
 * Export types and functions declared in board.hpp.
 */
void export_board() {
  using namespace boost::python;

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

