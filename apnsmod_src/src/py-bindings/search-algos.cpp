#include "search-algos.hpp"

#include <boost/python.hpp>

//! Export types and functions declared in search-algos.hpp.
void export_search_algos() {
  using namespace boost::python;

  def("bestSuccessor", &best_successor,
      return_internal_reference<>(),
      "bestSuccessor(Vertex) -> Vertex\n\nReturns the best of all vertex's successors, or None if the given vertex is a leaf.");

  class_<proof_number_search, boost::noncopyable>(
    "ProofNumberSearch",
    "The basic variant of the Proof-Number Search algorithm",
    init<boost::shared_ptr< ::game> const&>())
    
    .add_property("game", &proof_number_search::get_game,
                  "The Game object associated with this algorithm")
    .def("finished", &proof_number_search::finished,
         "pns.finished() -> bool\n\nHas the algorithm finished?")
    .def("iterate", &proof_number_search::iterate,
         "pns.iterate() -> None\n\nPerform a single iteration of the algorithm")
    .def("run", &proof_number_search::run,
         "pns.run(msHowLong) -> None\n\nRun the algorithm for msHowLong milliseconds. If msHowLong is 0, the algorithm will run"
         "until it finishes.")
    ;
}

