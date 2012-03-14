#include "tree.hpp"

#include <boost/python.hpp>
#include <boost/make_shared.hpp>

namespace {

//! Setter for vertex::step.
void vertex_set_step(vertex& vertex, boost::python::object step) {
  using namespace boost::python;

  if (!step.is_none())
    vertex.step = extract< ::step>(step)();
  else
    vertex.step = boost::none;
}

//! Wrapper around traverser to make it more Python-friendly.
class py_traverser {
  struct py_traversal_policy : virtual_traversal_policy::base {
    explicit py_traversal_policy(boost::python::object policy) : policy(policy) { }
    virtual vertex::children_iterator next(vertex& v) {
      using namespace boost::python;

      object n = policy(boost::ref(v));
      return extract<vertex::children_iterator>(n);
    }

  private:
    boost::python::object policy;
  };

  struct py_visitor : virtual_visitor::base {
    explicit py_visitor(boost::python::object visitor = boost::python::object()) : visitor(visitor) { }
    virtual void visit(vertex& v) {
      visitor(boost::ref(v));
    }

  private:
    boost::python::object visitor;
  };

public:
  typedef ::traverser<virtual_traversal_policy, virtual_visitor> traverser_t;

  py_traverser(boost::python::object traversal_policy, boost::python::object visitor = boost::python::object()) :
    traverser(
      virtual_traversal_policy(boost::make_shared<py_traversal_policy>(py_traversal_policy(traversal_policy))),
      virtual_visitor(boost::make_shared<py_visitor>(py_visitor(visitor)))
    )
  { }

  vertex::children_iterator traverse(vertex& root) {
    return traverser.traverse(root);
  }

private:
  traverser_t traverser;
};

} // anonymous namespace

//! Export functions and types declared in tree.hpp.
void export_tree() {
  using namespace boost::python;

  {
    scope vertex_scope = class_<vertex, boost::noncopyable>(
      "Vertex", "A single vertex of a search tree")
      .def_readonly("maxNum", &vertex::max_num, "Maximum possible value of proof- and disproof-numbers")
      .def_readonly("infty", &vertex::infty, "Infinity value for proof- and disproof-numbers")

      .def_readwrite("proofNumber", &vertex::proof_number, "The proof number associated with this vertex")
      .def_readwrite("disproofNumber", &vertex::disproof_number, "The disproof number associated with this vertex")
      .add_property("step",
                    make_getter(&vertex::step),
                    &vertex_set_step,
                    "The step that leads from parent to this step")
      .def_readwrite("stepsRemaining", &vertex::steps_remaining, "How many steps until the end of move")
      .def_readwrite("type_", &vertex::type, "The type of this vertex; either AND, or OR")

      .add_property("children", range<return_internal_reference<> >(
          static_cast<vertex::children_iterator (vertex::*)()>(&vertex::children_begin),
          static_cast<vertex::children_iterator (vertex::*)()>(&vertex::children_end)))
      .add_property("childrenCount", &vertex::children_count)
      
      .def("addChild", &vertex::add_child,
           return_internal_reference<>(),
           "v.addChild() -> Vertex\n\nAdd a child to this vertex. Returns a reference to the new child")
      .def("removeChild", &vertex::remove_child,
           "v.removeChild(Vertex) -> None\n\nRemove a child of this vertex")
      .def("reserve", &vertex::reserve,
           "v.reserve(n) -> None\n\nReserve enough memory for this vertex to hold n children")
      .def("pack", &vertex::pack,
           "v.pack() -> None\n\nMake this vertex only use as much memory as it needs to")
      ;

    enum_<vertex::e_type>("Type")
      .value("and_", vertex::type_and)
      .value("or_", vertex::type_or)
      ;
  }

  def("oppositeType", &opposite_type, "oppositeType(Vertex.Type) -> Vertex.Type\n\nGet the type opposite to the given one");

  class_<game, boost::noncopyable>("Game", "Container for the game and its search tree",
                                   init<board, piece::color_t>())
    .def_readonly("root", &game::root,
                  "Root of the search tree for this game")
    .def_readwrite("attacker", &game::attacker, "Colour of the attacking player")
    .def_readwrite("initialState", &game::initial_state, "Intial game state")
    ;

  class_<py_traverser>("Traverser", "An algorithm that traverses a given tree using a given policy and invokes the given visitor"
                                    "on each vertex",
                       init<object, object>())
    .def(init<object>())
    .def("traverse", &py_traverser::traverse,
         return_internal_reference<2>(),
         "t.traverse(Vertex) -> Vertex\n\nTraverses the tree rooted at given vertex, calling the visitor on each visited node."
         "Returns the last visited vertex.")
    ;
}

