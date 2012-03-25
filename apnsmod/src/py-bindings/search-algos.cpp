#include "search-algos.hpp"

#include <boost/python.hpp>

namespace {

struct proof_number_search_wrap : public proof_number_search {
  proof_number_search_wrap(PyObject*, boost::shared_ptr< ::game> const& game, unsigned position_count = 1) :
    proof_number_search(game, position_count)
  { }
};

} // anonymous namespace

//! Export types and functions declared in search-algos.hpp.
void export_search_algos() {
  using namespace boost::python;

  def("bestSuccessor", &best_successor,
      return_internal_reference<>(),
      "bestSuccessor(Vertex) -> Vertex\n\nReturns the best of all vertex's successors, or None if the given vertex is a leaf.");

  class_<search_algo<proof_number_search>, proof_number_search_wrap, boost::noncopyable>("ProofNumberSearch", 
    "The basic variant of the Proof-Number Search algorithm",
    init<boost::shared_ptr< ::game> const&>())
    .def(init<boost::shared_ptr< ::game> const&, unsigned>())
    .add_property("game", &proof_number_search::get_game,
                  "The Game object associated with this algorithm")
    .add_property("finished", &proof_number_search::finished,
                  "Has the algorithm finished?")
    .def("iterate", &proof_number_search::iterate,
         "algo.iterate() -> None\n\nPerform a single iteration of the algorithm")
    .def("run", &proof_number_search::run,
         "algo.run(msHowLong) -> None\n\nRun the algorithm for msHowLong milliseconds. If msHowLong is 0, the algorithm will run"
         "until it finishes.")
    .def("useTransTbl", &proof_number_search::use_trans_tbl,
         "pns.useTransTbl(size, keepTime) -> None\n\nMake this algorithm use a transposition table of given size and keep time.")
    
    .def("doIterate", pure_virtual(&proof_number_search::do_iterate),
         "Actually perform the iteration of the algorithm.")

    .add_property("transpositionTable", 
                  make_function(&proof_number_search::get_trans_tbl, return_internal_reference<>()),
                  "The TranspositionTable instance associated with this algorithm or None.")
    .add_property("positionCount",
                  &proof_number_search::get_position_count,
                  "Total number of vertices currently held by this algorithm")
    ;
}

