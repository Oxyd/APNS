#include "search-algos.hpp"

#include <boost/python.hpp>

namespace {

struct proof_number_search_wrap : public apns::proof_number_search {
  proof_number_search_wrap(PyObject*, boost::shared_ptr<apns::game> const& game, unsigned position_count = 1) :
    proof_number_search(game, position_count)
  { }
};

struct depth_first_pns_wrap : public apns::depth_first_pns {
  depth_first_pns_wrap(PyObject*, boost::shared_ptr<apns::game> const& game, unsigned position_count = 1) :
    depth_first_pns(game, position_count)
  { }
};

template <typename Algo, typename Wrapper>
void export_algo(char const* name, char const* description) {
  using namespace boost::python;

  class_<Algo, Wrapper, boost::noncopyable>(name, description,
                                            init<boost::shared_ptr<apns::game> const&>())
    .def(init<boost::shared_ptr<apns::game> const&, unsigned>())
    .add_property("game", &Algo::get_game,
                  "The Game object associated with this algorithm")
    .add_property("finished", &Algo::finished,
                  "Has the algorithm finished?")
    .def("iterate", &Algo::iterate,
         "algo.iterate() -> None\n\nPerform a single iteration of the algorithm")
    .def("run", &Algo::run,
         "algo.run(msHowLong) -> None\n\nRun the algorithm for msHowLong milliseconds. If msHowLong is 0, the algorithm will run"
         "until it finishes.")
    .def("useTransTbl", &Algo::use_trans_tbl,
         "algo.useTransTbl(size) -> None\n\n"
         "Make this algorithm use a transposition table of given size.")
    .def("useProofTbl", &Algo::use_proof_tbl,
         "algo.useProofTbl(size) -> None\n\n"
         "Make this algorithm use a proof table of given size.")
    
    .add_property("transpositionTable", 
                  make_function(&Algo::get_trans_tbl, return_internal_reference<>()),
                  "The TranspositionTable instance associated with this algorithm or None.")
    .add_property("proofTable",
                  make_function(&Algo::get_proof_tbl, return_internal_reference<>()),
                  "The ProofTable instance associated with this algorithm or None.")
    .add_property("positionCount",
                  &Algo::get_position_count,
                  "Total number of vertices currently held by this algorithm")
    .add_property("moveCacheSize",
                  static_cast<std::size_t (Algo::*)() const>(&Algo::move_cache_size),
                  static_cast<void (Algo::*)(std::size_t)>(&Algo::move_cache_size),
                  "Size of the moves cache.")
    .add_property("moveCacheHits",
                  &Algo::move_cache_hits,
                  "Number of hits in the move cache.")
    .add_property("moveCacheMisses",
                  &Algo::move_cache_misses,
                  "Number of misses in the move cache.")
    ;
}

} // anonymous namespace

//! Export types and functions declared in search-algos.hpp.
void export_search_algos() {
  using namespace boost::python;

  def("bestSuccessor", static_cast<apns::vertex* (*)(apns::vertex&)>(&apns::best_successor),
      return_internal_reference<>(),
      "bestSuccessor(Vertex) -> Vertex\n\nReturns the best of all vertex's successors, or None if the given vertex is a leaf.");

  export_algo<apns::search_algo<apns::proof_number_search>, proof_number_search_wrap>(
    "ProofNumberSearch",
    "The basic variant of the Proof-Number Search algorithm"
  );

  export_algo<apns::search_algo<apns::depth_first_pns>, depth_first_pns_wrap>(
    "DepthFirstPNS",
    "Depth-First variant of the algorithm"
  );
}

