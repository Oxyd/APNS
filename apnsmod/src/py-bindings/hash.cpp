#include "hash.hpp"
#include "tree.hpp"

#include <boost/python.hpp>

namespace {

apns::zobrist_hasher::hash_t zobrist_hasher_update(
  apns::zobrist_hasher const& hasher,
  apns::zobrist_hasher::hash_t original,
  std::vector<apns::elementary_step> const& steps,
  unsigned current_steps_remaining, unsigned next_steps_remaining,
  apns::piece::color_t from, apns::piece::color_t to)
{
  return hasher.update(original, steps.begin(), steps.end(), current_steps_remaining, next_steps_remaining, from, to);
}

} // anonymous namespace

//! Export functions and types declared in hash.hpp
void export_hash() {
  using namespace boost::python;

  class_<apns::zobrist_hasher>("ZobristHasher", "The Zobrist's hashing algorithm")
      .def("generateInitial", &apns::zobrist_hasher::generate_initial,
          "z.generateInitial(Board, Color) -> hash\n\n"
          "Generate the initial hash value for given board, assuming the specified player is on move")
      .def("update", &zobrist_hasher_update,
          "z.update(hash, [ElementaryStep], Color, Color) -> hash\n\n"
          "Update the hash value.")
      ;

  class_<apns::transposition_table, boost::noncopyable>(
    "TranspositionTable", "Transposition table", init<std::size_t, std::size_t>())
    .def_readonly("sizeOfElement", &apns::transposition_table::SIZE_OF_ELEMENT)

    .def("insert", &apns::transposition_table::insert,
        "t.insert(Hash, (Vertex.Number, Vertex.Number)) -> None\n\n"
        "Insert a vertex into the table")
    .def("query", &apns::transposition_table::query,
        "t.query(Hash) -> (Vertex.Number, Vertex.Number)\n\n"
        "Find a vertex in the table by the key. Return None if the vertex doesn't exist in the table")
    .def("tick", &apns::transposition_table::tick,
        "t.tick() -> None\n\n"
        "Update the internal tick count.")

    .add_property("memoryUsage",
        &apns::transposition_table::get_memory_usage,
        "Return the number of bytes used by the transposition table")
    .add_property("size", &apns::transposition_table::get_table_size,
                  "Maximal number of elements storeable in this table.")
    .add_property("elements",
        &apns::transposition_table::get_elements,
        "Return the number of elements stored in the table")
    .add_property("hits",
        &apns::transposition_table::get_hits,
        "The number of successful retreivals from the table")
    .add_property("misses",
        &apns::transposition_table::get_misses,
        "The number of unsuccsessful retreival attempts from the table")
    ;

  {
    scope s = class_<apns::history_table, boost::noncopyable>(
      "HistoryTable", "History table", init<std::size_t, std::size_t>())
      .def_readonly("sizeOfElement", &apns::history_table::SIZE_OF_ELEMENT)

      .def("insert", &apns::history_table::insert,
          "t.insert(Hash, History, (Vertex.Number, Vertex.Number)) -> None\n\n"
          "Insert a vertex into the table")
      .def("query", &apns::history_table::query,
          "t.query(Hash, History) -> (Vertex.Number, Vertex.Number)\n\n"
          "Find a vertex in the table by the key. Return None if the vertex doesn't exist in the table")
      .def("tick", &apns::history_table::tick,
          "t.tick() -> None\n\n"
          "Update the internal tick count.")

      .add_property("memoryUsage",
          &apns::history_table::get_memory_usage,
          "Return the number of bytes used by the transposition table")
      .add_property("size", &apns::history_table::get_table_size,
                    "Maximal number of elements storeable in this table.")
      .add_property("elements",
          &apns::history_table::get_elements,
          "Return the number of elements stored in the table")
      .add_property("hits",
          &apns::history_table::get_hits,
          "The number of successful retreivals from the table")
      .add_property("misses",
          &apns::history_table::get_misses,
          "The number of unsuccsessful retreival attempts from the table")
      ;

    class_<apns::history_table::history_record>("HistoryRecord")
      .def_readwrite("position", &apns::history_table::history_record::position)
      .def_readwrite("onMove", &apns::history_table::history_record::on_move)
      ;
  }
}

