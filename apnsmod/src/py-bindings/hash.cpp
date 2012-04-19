#include "hash.hpp"
#include "tree.hpp"

#include <boost/python.hpp>

namespace {

apns::zobrist_hasher::hash_t zobrist_hasher_update(
  apns::zobrist_hasher const& hasher,
  apns::zobrist_hasher::hash_t original,
  std::vector<apns::elementary_step> const& steps,
  apns::piece::color_t from, apns::piece::color_t to)
{
  return hasher.update(original, steps.begin(), steps.end(), from, to);
}

template <typename Table>
void export_table(char const* name, char const* description) {
  using namespace boost::python;

  class_<Table, boost::noncopyable>(
    name, description, init<std::size_t>())
    .def_readonly("sizeOfElement", &Table::SIZE_OF_ELEMENT)

    .def("insert", &Table::insert,
        "t.insert(Hash, (Vertex.Number, Vertex.Number)) -> None\n\n"
        "Insert a vertex into the table")
    .def("query", &Table::query,
        "t.query(Hash) -> (Vertex.Number, Vertex.Number)\n\n"
        "Find a vertex in the table by the key. Return None if the vertex doesn't exist in the table")

    .add_property("memoryUsage",
        &Table::memory_usage,
        "Return the number of bytes used by the transposition table")
    .add_property("size", &Table::table_size,
                  "Maximal number of elements storeable in this table.")
    .add_property("elements",
        &Table::elements,
        "Return the number of elements stored in the table")
    .add_property("hits",
        &Table::hits,
        "The number of successful retreivals from the table")
    .add_property("misses",
        &Table::misses,
        "The number of unsuccsessful retreival attempts from the table")
    ;

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

  export_table<apns::transposition_table>("TranspositionTable", "");
  export_table<apns::proof_table>("ProofTable", "");
}

