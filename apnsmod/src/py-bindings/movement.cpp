#include "movement.hpp"
#include "py-utils.hpp"

#include <boost/optional.hpp>
#include <boost/python.hpp>

/**
 * \brief Export types and functions declared in movement.hpp.
 */
void export_movement() {
  using namespace boost::python;

  class_<elementary_step>("ElementaryStep",
    "An elementary step is a displacement of a single piece on the board or a capture of a piece."
    "where is only meaningful if isCapture == false. If isCapture == true, where contains some arbitrary value,"
    "and 'from' are the coordinates of the piece just before capture.",
    no_init)
    .def("displacement", &elementary_step::displacement,
        "ElementaryStep.displacement(Position, Direction) -> ElementaryStep\n\n"
        "Create an elementary step representing a displacement of a piece from the given position in the given"
        "direction.")
    .staticmethod("displacement")

    .def("capture", &elementary_step::capture,
        "ElementaryStep.capture(Position) -> ElementaryStep\n\n"
        "Create an elementary step representing a capture from the given position.")
    .staticmethod("capture")

    .add_property("from", &elementary_step::get_from,
        "For displacement, this is the initial position of the to-be-moved piece. For capture, this is the position"
        "from which the piece will be captured.")
    .add_property("where", &elementary_step::get_where,
        "For displacement, this is where the piece will be moved. For capture, this field has no meaning and contains"
        "an arbitrary value.")
    .add_property("isCapture", &elementary_step::is_capture,
        "If true, this is a capture move. Otherwise, this is a displacement move.")
    .add_property("what", &elementary_step::get_what,
        "What piece is being moved/captured? This field may be None.")
    ;

  class_<step>("Step",
    "A step is a sequence of one or more elementary steps. It is created from displacements, then checked for validity "
    "and, if valid, it is checked for captures which are also inserted into the sequence.",
    no_init)
    .def("validateOrdinaryStep", &step::validate_ordinary_step,
        "Step.validateOrdinaryStep(Board, ElementaryStep) -> Step\n\n"
        "Check if an ordinary step (that is, neither push nor pull) conforms to the Arimaa game rules. If so, "
        "check for possible captures, construct the full step and return it. If the step is not valid, return None.")
    .staticmethod("validateOrdinaryStep")

    .def("validatePush", &step::validate_push,
        "Step.validatePush(Board, ElementaryStep, ElementaryStep) -> Step\n\n"
        "Similar to validateOrdinaryStep, except this validates a push move. The first ElementaryStep describes the "
        "movement of the pushed piece; the second one describes the pushing piece. If the step would not be valid, "
        "return None.")
    .staticmethod("validatePush")

    .def("validatePull", &step::validate_pull,
        "Step.validatePull(Board, ElementaryStep, ElementaryStep) -> Step\n\n"
        "Similar to validatePush, except this one validates a pull move. The first ElementaryStep describes the "
        "movement of the pulling piece; the second one describes the pulled piece. If the step would not be valid, "
        "return None")
    .staticmethod("validatePull")

    .def("fromString", &step::from_string,
        "Step.fromString(s) -> Step\n\n"
        "Create a step from its string representation using the official Arimaa step representation. If the string could"
        "not be parsed correctly as a step, return None. Note that this function does not validate whether the resulting"
        "step is valid for any board.")
    .staticmethod("fromString")

    .def("capture", &step::capture,
        "s.capture() -> bool\n\n"
        "Does the step contain a capture?")

    .def("toString", &step::to_string,
        "Get the string representation of this step")
    ;

  def("apply", &apply,
      "apply(Step, Board) -> None\n\n"
      "Apply given step to given board.");

  def("opponentColor", &opponent_color,
      "opponentColor(Color) -> Color\n\n"
      "Given a player's color, return the opponent's color.");

  to_python_converter<boost::optional<step>, optional_to_T<step> >();
}

