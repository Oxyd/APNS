#include "hash.hpp"
#include "tree.hpp"

#include <boost/python.hpp>

namespace {

/**
* Wrapper around #transposition_table<>::create to be usable as \c TranspositionTable_XX.__new__.
*/
template <typename Hasher>
typename transposition_table<Hasher>::pointer transposition_table_new(boost::python::object /*class*/,
    std::size_t table_size, std::size_t keep_time) {
  return transposition_table<Hasher>::create(table_size, keep_time);
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
        "Find a vertex in the table by the key. Return None if the vertex doesn't exist in the table",
        return_internal_reference<>())
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

} // anonymous namespace

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

