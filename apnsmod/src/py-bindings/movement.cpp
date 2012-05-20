#include "movement.hpp"
#include "py-utils.hpp"

#include <boost/optional.hpp>
#include <boost/python.hpp>

namespace {

//! Convert a step_holder to Step or None.
struct step_holder_converter {
  static PyObject* convert(apns::step_holder const& holder) {
    using namespace boost::python;

    if (holder)
      return incref(object(*holder).ptr());
    else
      return incref(Py_None);
  }
};

//! Wrapper to turn the steps_iterator into a Python iterator.
struct steps_iterator {
  explicit steps_iterator(apns::steps_iter const& iter) :
    steps_iter(iter)
  { }

  steps_iterator(apns::position what, apns::board const& position) :
    steps_iter(what, position)
  { }

  steps_iterator iter() const { return *this; }
  boost::python::object next() {
    using namespace boost::python;

    if (steps_iter != apns::steps_end())
      return object(*steps_iter++);
    else {
      PyErr_SetNone(PyExc_StopIteration);
      throw_error_already_set();
      return object();
    }
  }

private:
  apns::steps_iter steps_iter;
};

//! Create a StepsIterator.
steps_iterator steps(apns::position piece, apns::board const& board) {
  return steps_iterator(apns::steps_iter(piece, board));
}

//! Wrapper to turn the all_steps_iterator into a Python iterator.
struct all_steps_iterator {
  explicit all_steps_iterator(apns::all_steps_iter const& iter) :
    all_steps_iter(iter)
  { }

  all_steps_iterator(apns::board const& position, apns::piece::color_t player) :
    all_steps_iter(position, player)
  { }

  all_steps_iterator iter() const { return *this; }
  boost::python::object next() {
    using namespace boost::python;

    if (all_steps_iter != apns::all_steps_end())
      return object(*all_steps_iter++);
    else {
      PyErr_SetNone(PyExc_StopIteration);
      throw_error_already_set();
      return object();
    }
  }

private:
  apns::all_steps_iter all_steps_iter;
};

//! Create an AllStepsIterator.
all_steps_iterator all_steps(apns::board const& position, apns::piece::color_t player) {
  return all_steps_iterator(apns::all_steps_iter(position, player));
}

} // anonymous namespacek

/**
 * \brief Export types and functions declared in movement.hpp.
 */
void export_movement() {
  using namespace boost::python;

  to_python_converter<apns::step_holder, step_holder_converter>();

  class_<apns::elementary_step>("ElementaryStep",
    "An elementary step is a displacement of a single piece on the board or a capture of a piece."
    "where is only meaningful if isCapture == false. If isCapture == true, where contains some arbitrary value,"
    "and 'from' are the coordinates of the piece just before capture.",
    no_init)
    .def("displacement", &apns::elementary_step::displacement,
        "ElementaryStep.displacement(Position, Direction) -> ElementaryStep\n\n"
        "Create an elementary step representing a displacement of a piece from the given position in the given"
        "direction.")
    .staticmethod("displacement")

    .def("capture", &apns::elementary_step::capture,
        "ElementaryStep.capture(Position) -> ElementaryStep\n\n"
        "Create an elementary step representing a capture from the given position.")
    .staticmethod("capture")

    .add_property("from", &apns::elementary_step::from,
        "For displacement, this is the initial position of the to-be-moved piece. For capture, this is the position"
        "from which the piece will be captured.")
    .add_property("where", &apns::elementary_step::where,
        "For displacement, this is where the piece will be moved. For capture, this field has no meaning and contains"
        "an arbitrary value.")
    .add_property("isCapture", &apns::elementary_step::capture,
        "If true, this is a capture move. Otherwise, this is a displacement move.")
    .add_property("what", &apns::elementary_step::what,
        "What piece is being moved/captured? This field may be None.")
    ;

  class_<apns::step>("Step",
    "A step is a sequence of one or more elementary steps. It is created from displacements, then checked for validity "
    "and, if valid, it is checked for captures which are also inserted into the sequence.",
    no_init)
    .def("validateOrdinaryStep", &apns::step::validate_ordinary_step,
        "Step.validateOrdinaryStep(Board, ElementaryStep) -> Step\n\n"
        "Check if an ordinary step (that is, neither push nor pull) conforms to the Arimaa game rules. If so, "
        "check for possible captures, construct the full step and return it. If the step is not valid, return None.")
    .staticmethod("validateOrdinaryStep")

    .def("validatePush", &apns::step::validate_push,
        "Step.validatePush(Board, ElementaryStep, ElementaryStep) -> Step\n\n"
        "Similar to validateOrdinaryStep, except this validates a push move. The first ElementaryStep describes the "
        "movement of the pushed piece; the second one describes the pushing piece. If the step would not be valid, "
        "return None.")
    .staticmethod("validatePush")

    .def("validatePull", &apns::step::validate_pull,
        "Step.validatePull(Board, ElementaryStep, ElementaryStep) -> Step\n\n"
        "Similar to validatePush, except this one validates a pull move. The first ElementaryStep describes the "
        "movement of the pulling piece; the second one describes the pulled piece. If the step would not be valid, "
        "return None")
    .staticmethod("validatePull")

    .def("fromString", &apns::step::from_string,
        "Step.fromString(s) -> Step\n\n"
        "Create a step from its string representation using the official Arimaa step representation. If the string could"
        "not be parsed correctly as a step, return None. Note that this function does not validate whether the resulting"
        "step is valid for any board.")
    .staticmethod("fromString")

    .def("capture", &apns::step::capture,
        "s.capture() -> bool\n\n"
        "Does the step contain a capture?")

    .def("toString", &apns::step::to_string,
        "Get the string representation of this step")
    
    .add_property("stepsUsed", &apns::step::steps_used,
                  "How many steps this steps counts as? Ordinary steps count as one, pushes and pulls as two. Captures are "
                  "not counted.")

    .add_property("elementarySteps",
                  range(&apns::step::step_sequence_begin,
                        &apns::step::step_sequence_end),
                  "Elementary steps that make up this whole step.")

    .def("__str__", &apns::step::to_string)
    .def("__repr__", &apns::step::to_string)
    ;

  def("apply", &apns::apply,
      "apply(Step, Board) -> None\n\n"
      "Apply given step to given board.");

  def("unapply", &apns::unapply,
      "unapply(Step, Board) -> None\n\n"
      "Undo the application of the step on the board");

  def("opponentColor", &apns::opponent_color,
      "opponentColor(Color) -> Color\n\n"
      "Given a player's color, return the opponent's color.");

  to_python_converter<boost::optional<apns::step>, optional_to_T<apns::step> >();

  class_<steps_iterator>("StepsIterator", init<apns::position, apns::board const&>())
    .def("__iter__", &steps_iterator::iter)
    .def("next", &steps_iterator::next)
    ;

  def("steps", &steps,
      "steps(Position, Board) -> [Step]\n\n"
      "Create an iterator object that goes through all possible steps for given piece on given board.");

  class_<all_steps_iterator>("AllStepsIterator", init<apns::board const&, apns::piece::color_t>())
    .def("__iter__", &all_steps_iterator::iter)
    .def("next", &all_steps_iterator::next)
    ;

  def("allSteps", &all_steps,
      "allSteps(Board, Color) -> [Step]\n\n"
      "Create an iteratoor object that goes through all possible steps for a given player from a given position");

  scope().attr("MAX_STEPS") = apns::MAX_STEPS;
}

