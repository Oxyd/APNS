#include "hash.hpp"
#include "tree.hpp"

#include <boost/python.hpp>

namespace {

zobrist_hasher::hash_t zobrist_hasher_update(
  zobrist_hasher const& hasher,
  zobrist_hasher::hash_t original,
  std::vector<elementary_step> const& steps,
  piece::color_t from, piece::color_t to)
{
  return hasher.update(original, steps.begin(), steps.end(), from, to);
}

} // anonymous namespace

//! Export functions and types declared in hash.hpp
void export_hash() {
  using namespace boost::python;

  class_<zobrist_hasher>("ZobristHasher", "The Zobrist's hashing algorithm")
      .def("generateInitial", &zobrist_hasher::generate_initial,
          "z.generateInitial(Board, Color) -> hash\n\n"
          "Generate the initial hash value for given board, assuming the specified player is on move")
      .def("update", &zobrist_hasher_update,
          "z.update(hash, [ElementaryStep], Color, Color) -> hash\n\n"
          "Update the hash value.")
      ;

  class_<transposition_table, boost::noncopyable>("TranspositionTable", "Transposition table",
                                                  init<std::size_t, std::size_t>())
    .def_readonly("sizeOfElement", &transposition_table::SIZE_OF_ELEMENT)

    .def("insert", &transposition_table::insert,
        "t.insert(Hash, Vertex) -> None\n\n"
        "Insert a vertex into the table")
    .def("query", &transposition_table::query,
        "t.query(Hash) -> Vertex\n\n"
        "Find a vertex in the table by the key. Return None if the vertex doesn't exist in the table")
    .def("tick", &transposition_table::tick,
        "t.tick() -> None\n\n"
        "Update the internal tick count.")

    .add_property("memoryUsage",
        &transposition_table::get_memory_usage,
        "Return the number of bytes used by the transposition table")
    .add_property("elements",
        &transposition_table::get_elements,
        "Return the number of elements stored in the table")
    .add_property("hits",
        &transposition_table::get_hits,
        "The number of successful retreivals from the table")
    .add_property("misses",
        &transposition_table::get_misses,
        "The number of unsuccsessful retreival attempts from the table")
    ;
}

