#include "tree.hpp"
#include "py-utils.hpp"

#include <boost/python.hpp>
#include <boost/optional.hpp>
#include <boost/make_shared.hpp>

namespace {

//! Setter for vertex::step.
void vertex_set_step(apns::vertex& vertex, boost::python::object step) {
  using namespace boost::python;

  if (!step.is_none())
    vertex.step = extract<apns::step>(step)();
  else
    vertex.step = apns::step_holder::none;
}

//! Wrapper around Traversal Policy to make it more Python-friendly.
struct py_traversal_policy : apns::virtual_traversal_policy::base {
  explicit py_traversal_policy(boost::python::object policy) : policy(policy) { }
  virtual apns::vertex* next(apns::vertex& v) {
    using namespace boost::python;

    object n = policy(boost::ref(v));
    return extract<apns::vertex*>(n);
  }

private:
  boost::python::object policy;
};

//! wrapper around Visitor to make it more python-friendly.
struct py_visitor : apns::virtual_visitor::base {
  explicit py_visitor(boost::python::object visitor = boost::python::object()) : visitor(visitor) { }
  virtual void visit(apns::vertex& v) {
    if (!visitor.is_none())
      visitor(boost::ref(v));
  }

private:
  boost::python::object visitor;
};

//! Wrapper around Stop Condition to make it more Python-friendly.
struct py_stop_condition : apns::virtual_stop_condition::base {
  explicit py_stop_condition(boost::python::object condition = boost::python::object()) : condition(condition) { }
  virtual bool stop(apns::vertex& v) {
    using namespace boost::python;

    if (!condition.is_none()) {
      object result = condition(boost::ref(v));
      return extract<bool>(result);
    } else
      return false;
  }

private:
  boost::python::object condition;
};

//! Wrapper around traverse to make it more Python-friendly.
apns::vertex* py_traverse(apns::vertex& v,
                          boost::python::object traversal_policy,
                          boost::python::object visitor,
                          boost::python::object stop_condition) {
  return apns::traverse(
    v,
    apns::virtual_traversal_policy(boost::make_shared<py_traversal_policy>(py_traversal_policy(traversal_policy))),
    apns::virtual_visitor(boost::make_shared<py_visitor>(py_visitor(visitor))),
    apns::virtual_stop_condition(boost::make_shared<py_stop_condition>(py_stop_condition(stop_condition)))
  );
}

apns::vertex* py_traverse(apns::vertex& v,
                          boost::python::object traversal_policy,
                          boost::python::object visitor) {
  return py_traverse(v, traversal_policy, visitor, boost::python::object());
}

apns::vertex* py_traverse(apns::vertex& v, boost::python::object traversal_policy) {
  using namespace boost::python;
  return py_traverse(v, traversal_policy, object(), object());
}

std::ptrdiff_t vertex_hash(apns::vertex& v) {
  return reinterpret_cast<std::ptrdiff_t>(&v);
}

apns::vertex* vertex_add_child(apns::vertex& parent) {
  return &*parent.add_child();
}

void vertex_remove_child(apns::vertex& parent, apns::vertex* child) {
  parent.remove_child(parent.iter_from_ptr(child));
}

apns::vertex* game_get_root(apns::game& g) {
  return &g.root;
}

} // anonymous namespace

//! Export functions and types declared in tree.hpp.
void export_tree() {
  using namespace boost::python;

  {
    scope vertex_scope = class_<apns::vertex, boost::noncopyable>(
      "Vertex", "A single vertex of a search tree")
      .def_readonly("maxNum", &apns::vertex::max_num, "Maximum possible value of proof- and disproof-numbers")
      .def_readonly("infty", &apns::vertex::infty, "Infinity value for proof- and disproof-numbers")
      .add_static_property("allocSize", &apns::vertex::alloc_size,
                           "Total number of bytes allocated by all Vertices in the program")

      .def_readwrite("proofNumber", &apns::vertex::proof_number, "The proof number associated with this vertex")
      .def_readwrite("disproofNumber", &apns::vertex::disproof_number, "The disproof number associated with this vertex")
      .add_property("step",
                    make_getter(&apns::vertex::step, return_value_policy<return_by_value>()),
                    &vertex_set_step,
                    "The step that leads from parent to this vertex.")
      .def_readwrite("stepsRemaining", &apns::vertex::steps_remaining, "How many steps until the end of move")
      .def_readwrite("type_", &apns::vertex::type, "The type of this vertex; either AND, or OR")

      .add_property("children", range<return_internal_reference<> >(
          static_cast<apns::vertex::children_iterator (apns::vertex::*)()>(&apns::vertex::children_begin),
          static_cast<apns::vertex::children_iterator (apns::vertex::*)()>(&apns::vertex::children_end)))
      .add_property("childrenCount", &apns::vertex::children_count)
      .add_property("leaf", &apns::vertex::leaf)
      
      .def("addChild", &vertex_add_child,
           return_internal_reference<>(),
           "v.addChild() -> Vertex\n\nAdd a child to this vertex. Returns a reference to the new child")
      .def("removeChild", &vertex_remove_child,
           "v.removeChild(Vertex) -> None\n\nRemove a child of this vertex")
      .def("resize", &apns::vertex::resize,
           "v.resize(n) -> None\n\nResize this vertex to have exactly n children. If that means to shrink this vertex, the "
           "children at the end are removed.")
      .def("reserve", &apns::vertex::reserve,
           "v.reserve(n) -> None\n\nReserve enough memory for this vertex to hold n children")
      .def("pack", &apns::vertex::pack,
           "v.pack() -> None\n\nMake this vertex only use as much memory as it needs to")

      .def("__hash__", &vertex_hash)
      ;

    enum_<apns::vertex::e_type>("Type")
      .value("and_", apns::vertex::type_and)
      .value("or_", apns::vertex::type_or)
      ;
  }

  def("oppositeType", &apns::opposite_type, "oppositeType(Vertex.Type) -> Vertex.Type\n\nGet the type opposite to the given one");

  class_<apns::game, boost::noncopyable, boost::shared_ptr<apns::game> >(
    "Game", "Container for the game and its search tree",
    init<apns::board, apns::piece::color_t>())
    .def_readonly("root", &apns::game::root,
                  "Root of the search tree for this game")
    .def_readwrite("attacker", &apns::game::attacker, "Colour of the attacking player")
    .def_readwrite("initialState", &apns::game::initial_state, "Intial game state")
    ;

  void (*save_game1)(boost::shared_ptr<apns::game> const&, std::string const&, apns::operation_controller&) = &apns::save_game;
  void (*save_game2)(boost::shared_ptr<apns::game> const&, std::string const&) = &apns::save_game;
  def("saveGame", save_game1,
      "saveGame(Game, filename [, OperationController]) -> None\n\n"
      "Save the game to a file. This algorithm accepts an "
      "OperationController so that its execution may be cancelled, but it does not provide any information "
      "about its progress");
  def("saveGame", save_game2);

  std::pair<boost::shared_ptr<apns::game>, std::size_t> (*load_game1)(std::string const&, apns::operation_controller&) = &apns::load_game;
  std::pair<boost::shared_ptr<apns::game>, std::size_t> (*load_game2)(std::string const&) = &apns::load_game;
  def("loadGame", load_game1,
      "loadGame(filename [, OperationController]) -> (Game, vertexCount)\n\n"
      "Load the game from a file. Returns a new Game object. This algorithm accepts an "
      "OperationController so that its execution may be cancelled, but it does not provide any information "
      "about its progress");
  def("loadGame", load_game2);

  to_python_converter<
    std::pair<boost::shared_ptr<apns::game>, std::size_t>,
    pair_to_tuple<boost::shared_ptr<apns::game>, std::size_t> 
  >();

  apns::vertex* (*py_traverse1)(apns::vertex&, object, object, object) = &py_traverse;
  apns::vertex* (*py_traverse2)(apns::vertex&, object, object) = &py_traverse;
  apns::vertex* (*py_traverse3)(apns::vertex&, object) = &py_traverse;
  def("traverse", py_traverse1,
      return_internal_reference<1>(),
      "traverse(Vertex, traversalPolicy, visitor, stopCondition) -> Vertex\n\n"
      "Traverse the tree in a way specified by the traversal policy. On each vertex calls the visitor. After each call to the "
      "visitor, stopCondition is called on the same vertex -- if that returns True, the traversal is stopped.");
  def("traverse", py_traverse2, return_internal_reference<1>());
  def("traverse", py_traverse3, return_internal_reference<1>());
}

