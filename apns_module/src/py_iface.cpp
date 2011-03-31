/**
 * \file py_iface.cpp
 *
 * \brief C++/Python interface.
 *
 * The glue between C++ and Python code. This file defines the entry initialisation function which
 * exports public C++ types and functions for use from Python. There are certain differences between C++ and Python
 * identifiers. Namely, the C++ code uses the all_lowercase convention, while Python uses CamelCase for types and
 * lowerCamelCase for functions and variables.
 */

#include "board.hpp"
#include "movement.hpp"
#include "search.hpp"
#include "hash.hpp"

#include <boost/python.hpp>
#include <boost/optional.hpp>
#include <boost/multi_array.hpp>

#include <string>

namespace {

//! Convert an \c int to #vertex::e_type.
vertex::e_type vertex_type_from_int(int t) {
  switch (t) {
  case vertex::type_and:    return vertex::type_and;
  case vertex::type_or:     return vertex::type_or;
  default:
    throw std::domain_error("type_from_int: Could not convert integer to vertex type");
  }
}

//! Convert a C++ sequence into a Python list.
template <typename Iterator>
boost::python::list py_list_from_cpp_seq(Iterator begin, Iterator end) {
  using namespace boost::python;

  list result;
  while (begin != end) {
    result.append(*begin++);
  }

  return result;
}

//! Make a C++ sequence from a Python list.
template <typename Container>
void cpp_seq_from_py_list(boost::python::list const& list, Container& cont) {
  using namespace boost::python;

  for (int index = 0; index < len(list); ++index) {
    cont.insert(cont.end(),
        extract<typename Container::value_type>(list[index]));
  }
}

}

/**
 * \brief Converter from \c optional<T> to \c T or \c None.
 *
 * This is for use with \c to_python_converter. Any object \c o of type \c boost::optional<T> will be converted
 * either to \c None if \c o is empty, or to \c *o otherwise.
 */
template <typename T>
struct optional_to_T {
  static PyObject* convert(boost::optional<T> o) {
    using namespace boost::python;

    if (o) {
      return incref(object(*o).ptr());
    } else {
      return Py_None;
    }
  }
};

/**
 * \brief Converter from \c std::pair<A, B> to Python tuple.
 *
 * This is for use with \c to_python_converter. If \c p is a \c std::pair<A, B>, then \c p will be converted
 * to <tt>(p.first, p.second)</tt>.
 */
template <typename A, typename B>
struct pair_to_tuple {
  static PyObject* convert(std::pair<A, B> p) {
    using namespace boost::python;

    return incref(make_tuple(p.first, p.second).ptr());
  }
};

/**
 * Converter from \c boost::weak_ptr<T> to a Python object.
 *
 * If the \c weak_ptr doesn't hold the reference anymore, the converted value will be None.
 */
template <typename T>
struct weak_to_shared {
  static PyObject* convert(boost::weak_ptr<T> ptr) {
    using namespace boost::python;

    boost::shared_ptr<T> shared = ptr.lock();
    if (shared) {
      return incref(object(shared).ptr());
    } else {
      return Py_None;
    }
  }
};

/**
 * Python wrapper for operation_controller. Required to be able to inherit from it in Python and override virtual functions.
 */
class operation_controller_wrapper : public operation_controller, public boost::python::wrapper<operation_controller> {
  virtual void do_update() {
    get_override("doUpdate")();
  }
public:
  explicit operation_controller_wrapper(unsigned update_interval)
    : operation_controller(update_interval)
  { }
};

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

/**
 * Provide a hashing function for position so that Position is usable in Python dictionaries.
 */
unsigned position_hash(position pos) {
  return (pos.get_column() << 16) | pos.get_row();
}

/**
 * Hashing function for Vertex so that Vertex can be used in Python dictionaries.
 */
std::ptrdiff_t vertex_hash(vertex_ptr v) {
  return reinterpret_cast<std::ptrdiff_t>(v.get());
}

//! A setter for vertex::leading_step.
void set_vertex_leading_step(vertex_ptr vertex, boost::python::object step) {
  using namespace boost::python;

  if (step.is_none()) {
    vertex->leading_step = boost::optional< ::step>();
  } else {
    vertex->leading_step = extract< ::step>(step)();
  }
}

/**
 * Wrapper around #transposition_table<>::create to be usable as \c TranspositionTable_XX.__new__.
 */
template <typename Hasher>
typename transposition_table<Hasher>::pointer transposition_table_new(boost::python::object /*class*/,
    std::size_t table_size, std::size_t keep_time) {
  return transposition_table<Hasher>::create(table_size, keep_time);
}

//! Make a copy of a board instance. This is so that Python can explicitely ask for copies of board objects.
board board_copy(board const& original) {
  return original;
}

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

/**
 * \brief Export types and functions declared in movement.hpp.
 */
void export_movement() {
  using namespace boost::python;

  to_python_converter<boost::optional<step>, optional_to_T<step> >();

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
}

template <typename Strategy, typename Hasher>
void export_pn_search_algo(std::string const& identifier) {
  using namespace boost::python;

  class_<pn_search_algo<Strategy, Hasher>, boost::noncopyable>(identifier.c_str(),
      "The Proof-Number Search algorithm",
      init<board const&, piece::color_t, Strategy>()[
          with_custodian_and_ward<1, 2>()
      ])
      .def(init<board const&, int, Strategy>()[with_custodian_and_ward<1, 2>()])
      .def(init<board const&, vertex_ptr, piece::color_t, Strategy, unsigned>()[
          with_custodian_and_ward<1, 2,
          with_custodian_and_ward<1, 3> >()
      ])
      .def("run", &pn_search_algo<Strategy, Hasher>::run,
          "search.run(msHowLong) -> None\n\n"
          "Continue the search. The msHowLong parameter specifies how long the search should run, in milliseconds.")
      .add_property("root",
          make_function(&pn_search_algo<Strategy, Hasher>::get_root,
              return_internal_reference<>()))
      .add_property("finished", &pn_search_algo<Strategy, Hasher>::finished)
      .add_property("player", &pn_search_algo<Strategy, Hasher>::get_player)
      .add_property("positionCount", &pn_search_algo<Strategy, Hasher>::get_position_count)

      .def("getInitialBoard",
          &pn_search_algo<Strategy, Hasher>::get_initial_board,
          return_value_policy<copy_const_reference>())
      .add_property("initialBoard",
          make_function(&pn_search_algo<Strategy, Hasher>::get_initial_board,
              return_value_policy<copy_const_reference>()))
      .def("successor",
          &pn_search_algo<Strategy, Hasher>::successor)
      .def("iterate", &pn_search_algo<Strategy, Hasher>::iterate,
          "Perform one iteration of the algorithm")

      .add_static_property("sizeOfTransTblElement",
          &pn_search_algo<Strategy, Hasher>::get_size_of_trans_tbl_element,
          "Size of one element in the transposition table")
      .def("useTranspositionTable",
          &pn_search_algo<Strategy, Hasher>::use_transposition_table,
          "algo.useTranspositionTable(size, keepTime) -> None\n\n"
          "Instruct the algorithm to use a transposition table capable of holding 'size' elements which are kept for"
          "at most 'keepTime' collisions.")
      .def("getTranspositionTable",
          &pn_search_algo<Strategy, Hasher>::get_transposition_table,
          return_internal_reference<>(),
          "Get the used transposition table instance")
      ;
}

//! Export functions and types declared in search.hpp.
void export_search() {
  using namespace boost::python;

  to_python_converter<boost::weak_ptr<vertex>, weak_to_shared<vertex> >();

  {
    scope vertex_scope = class_<vertex, vertex_ptr>("Vertex", no_init)
        .def_readwrite("type_", &vertex::type, "Type of this vertex: either AND or OR")
        .add_property("children",
            range(&vertex::children_begin, &vertex::children_end),
            "Children of this vertex of either type")
        .def("addChild", &vertex::add_child, with_custodian_and_ward<1, 2>())
        .add_property("parents",
            range(&vertex::parents_begin, &vertex::parents_end),
            "Parents of this vertex")
        .add_property("leadingStep",
            make_getter(&vertex::leading_step, return_value_policy<return_by_value>()),
            &set_vertex_leading_step,
            "Which step leads to this state from parent")

        .def_readwrite("stepsRemaining", &vertex::steps_remaining, "How many steps until the end of a move")
        .def_readwrite("proofNumber", &vertex::proof_number, "Proof Number of this vertex")
        .def_readwrite("disproofNumber", &vertex::disproof_number, "Disproof number of this vertex")
        .def_readwrite("pickleNumber", &vertex::pickle_number)

        .def_readonly("infty", &vertex::infty, "Infinity value")
        .def_readonly("maxNum", &vertex::max_num, "Maximum acceptable value for a PN or DN")

        .def("create", static_cast<vertex_ptr (*)()>(&vertex::create))
        .staticmethod("create")

        .def("__hash__", &vertex_hash)
        ;

    enum_<vertex::e_type>("Type")
        .value("and_", vertex::type_and)
        .value("or_", vertex::type_or)
        ;
  }

  class_<win_strategy>("WinStrategy")
      .def("updatedNumbers", &win_strategy::updated_numbers)
      .def("initialNumbers", &win_strategy::initial_numbers)
      ;

  export_pn_search_algo<
    win_strategy,
    zobrist_hasher
  >("PnSearchAlgo_WinStrategy_ZobristHash");
}

//! Export an instantiation of TranspositionTable<H>.
template <typename Hash>
void export_trans_tbl(char const* name) {
  using namespace boost::python;

  class_<transposition_table<Hash>,
        boost::shared_ptr<transposition_table<Hash> >,
        boost::noncopyable
      >(name, "Transposition table", no_init)
      .def("insert", &transposition_table<Hash>::insert,
          "t.insert(Hash, Vertex) -> None\n\n"
          "Insert a vertex into the table")
      .def("query", &transposition_table<Hash>::query,
          "t.query(Hash) -> Vertex\n\n"
          "Find a vertex in the table by the key. Return None if the vertex doesn't exist in the table")
      .def("tick", &transposition_table<Hash>::tick,
          "t.tick() -> None\n\n"
          "Update the internal tick count.")

      .def("create", &transposition_table<Hash>::create,
          "T.create(size, keepTime) -> T\n\n"
          "Create a new TranspositionTable object")
      .staticmethod("create")

      .def("__new__", &transposition_table_new<Hash>)

      .add_property("memoryUsage",
          &transposition_table<Hash>::get_memory_usage,
          "Return the number of bytes used by the transposition table")
      .add_property("elements",
          &transposition_table<Hash>::get_elements,
          "Return the number of elements stored in the table")
      .add_property("hits",
          &transposition_table<Hash>::get_hits,
          "The number of successful retreivals from the table")
      .add_property("misses",
          &transposition_table<Hash>::get_misses,
          "The number of unsuccsessful retreival attempts from the table")
      ;
}

//! Export functions and types declared in hash.hpp
void export_hash() {
  using namespace boost::python;

  class_<zobrist_hasher>("ZobristHasher", "The Zobrist's hashing algorithm")
      .def("generateInitial", &zobrist_hasher::generate_initial,
          "z.generateInitial(Board, Color) -> hash\n\n"
          "Generate the initial hash value for given board, assuming the specified player is on move")
      .def("update",
          static_cast<
            zobrist_hasher::hash_t (zobrist_hasher::*)(zobrist_hasher::hash_t,
                                                       step::elementary_step_seq const&,
                                                       piece::color_t,
                                                       piece::color_t) const
          >(&zobrist_hasher::update),
          "z.update(hash, [ElementaryStep], Color, Color) -> hash\n\n"
          "Update the hash value.")
      ;

  export_trans_tbl<zobrist_hasher>("TranspositionTable_ZobristHasher");
}

//! Export functions and types declared in util.hpp
void export_util() {
  using namespace boost::python;

  def("memoryUsedTotal", &get_memory_usage,
      "Get the amount of total memory used for search in bytes.");

  class_<operation_controller_wrapper, boost::noncopyable>("OperationController",
      "Controller for long-running operations",
      init<unsigned>())
      .def("update", static_cast<void (operation_controller::*)()>(&operation_controller::update),
          "Update the controller and, possibly, the interface")
      .def("update", static_cast<void (operation_controller::*)(unsigned, unsigned)>(&operation_controller::update),
          "Update the controller, providing information about the progress")
      .def("requestStop", &operation_controller::request_stop,
          "Request that the algorithm stop")
      .add_property("stop", &operation_controller::stop, "Should the algorithm stop now?")
      .add_property("workDone", &operation_controller::get_work_done, "Get the amount of work done")
      .add_property("workTotal", &operation_controller::get_work_total, "Get the amount of work total")

      .def("doUpdate", pure_virtual(&operation_controller::do_update))
      ;

  def("dumpTree", &dump_tree);
  def("loadTree", &load_tree);
}

/**
 * \brief Python module initialization entry point.
 */
BOOST_PYTHON_MODULE(apnsmod) {
  export_board();
  export_movement();
  export_search();
  export_hash();
  export_util();
}
