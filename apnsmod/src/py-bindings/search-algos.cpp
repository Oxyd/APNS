#include "search-algos.hpp"

#include <boost/python.hpp>

namespace {

struct proof_number_search_wrap : public apns::proof_number_search {
  proof_number_search_wrap(
    PyObject*, boost::shared_ptr<apns::game> const& game,
    unsigned position_count = 1
  ) :
    proof_number_search(game, position_count)
  { }
};

struct depth_first_pns_wrap : public apns::depth_first_pns {
  depth_first_pns_wrap(
    PyObject*, boost::shared_ptr<apns::game> const& game,
    unsigned position_count = 1
  ) :
    depth_first_pns(game, position_count)
  { }
};

struct virtual_algo_wrap 
  : public apns::virtual_algo, boost::python::wrapper<apns::virtual_algo> 
{
  virtual_algo_wrap(boost::shared_ptr<apns::game> const& game,
                    unsigned position_count = 1) :
    virtual_algo(game, position_count)
  { }

  virtual_algo_wrap(PyObject*, boost::shared_ptr<apns::game> const& game,
                    unsigned position_count = 1) :
    virtual_algo(game, position_count)
    { }

  virtual void really_do_iterate() {
    this->get_override("doIterate")();
  }

  boost::shared_ptr<apns::game> game() { return game_; }
  apns::zobrist_hasher& hasher() { return hasher_; }
  apns::zobrist_hasher::hash_t initial_hash() { return initial_hash_; }
};

template <typename Algo, typename Base>
boost::python::class_<Algo, boost::python::bases<Base>, boost::noncopyable>
export_algo(char const* name, char const* description) {
  using namespace boost::python;

  std::size_t (Base::* get_gc_low)() const = &Base::gc_low;
  std::size_t (Base::* get_gc_high)() const = &Base::gc_high;
  void (Base::* set_gc_low)(std::size_t) = &Base::gc_low;
  void (Base::* set_gc_high)(std::size_t) = &Base::gc_high;

  std::string base_name = std::string("_") + typeid(Base).name();
  class_<Base, boost::noncopyable>(base_name.c_str(), no_init)
    .add_property("game", &Base::get_game,
                  "The Game object associated with this algorithm")
    .add_property("finished", &Base::finished,
                  "Has the algorithm finished?")
    .def("iterate", &Base::iterate,
         "algo.iterate() -> None\n\n"
         "Perform a single iteration of the algorithm")
    .def("run", &Base::run,
         "algo.run(msHowLong) -> None\n\n"
         "Run the algorithm for msHowLong milliseconds. If msHowLong is 0, "
         "the algorithm will run until it finishes.")
    .def("useTransTbl", &Base::use_trans_tbl,
         "algo.useTransTbl(size) -> None\n\n"
         "Make this algorithm use a transposition table of given size.")
    .def("useProofTbl", &Base::use_proof_tbl,
         "algo.useProofTbl(size) -> None\n\n"
         "Make this algorithm use a proof table of given size.")
    
    .add_property("transpositionTable", 
                  make_function(&Base::get_trans_tbl,
                                return_internal_reference<>()),
                  "The TranspositionTable instance associated with this "
                  "algorithm or None.")
    .add_property("proofTable",
                  make_function(&Base::get_proof_tbl,
                                return_internal_reference<>()),
                  "The ProofTable instance associated with this algorithm or "
                  "None.")
    .add_property("positionCount",
                  &Base::get_position_count,
                  "Total number of vertices currently held by this algorithm")
#if 0
    .add_property(
      "moveCacheSize",
      static_cast<std::size_t (Base::*)() const>(&Base::move_cache_size),
      static_cast<void (Base::*)(std::size_t)>(&Base::move_cache_size),
      "Size of the moves cache."
    )
    .add_property("moveCacheHits",
                  &Base::move_cache_hits,
                  "Number of hits in the move cache.")
    .add_property("moveCacheMisses",
                  &Base::move_cache_misses,
                  "Number of misses in the move cache.")
#endif
    .add_property("historyTblSize",
                  &Base::history_tbl_size,
                  "Number of steps remembered by the history table.")

    .add_property("gcLow",
                  get_gc_low, set_gc_low,
                  "The low threshold for garbage collector.")
    .add_property("gcHigh",
                  get_gc_high, set_gc_high,
                  "High threshold for garbage collector. Value of 0 disables "
                  "garbage collector completely.")

    .add_property("logSink",
                  &Base::log, &Base::log_into,
                  "Sink into which the algorithm shall output some debugging "
                  "information.")
    ;

  return class_<Algo, bases<Base>, boost::noncopyable>(
    name, description,
    init<boost::shared_ptr<apns::game> const&>()
  ) .def(init<boost::shared_ptr<apns::game> const&, unsigned>())
    ;
}

template <typename Algo>
boost::python::class_<
  Algo,
  boost::python::bases<apns::search_algo<Algo> >,
  boost::noncopyable
>
export_algo(char const* name, char const* description) {
  return export_algo<Algo, apns::search_algo<Algo> >(name, description);
}

} // anonymous namespace

//! Export types and functions declared in search-algos.hpp.
void export_search_algos() {
  using namespace boost::python;

  def("bestSuccessor",
      static_cast<apns::vertex* (*)(apns::vertex&)>(&apns::best_successor),
      return_internal_reference<>(),
      "bestSuccessor(Vertex) -> Vertex\n\n"
      "Returns the best of all vertex's successors, or None if the given "
      "vertex is a leaf.");

  class_<apns::history_table>("HistoryTable",
                              "Remembers steps that caused cutoff and uses "
                              "that information for step-ordering.")
    .def("insert", &apns::history_table::insert,
         "h.insert(Step, depth) -> None\n\n"
         "Tell the table that given step caused cutoff at given depth.")
    .def("sort", &apns::history_table::sort,
         "h.sort(Vertex) -> None\n\n"
         "Sort children of the vertex.")
    .add_property("size", &apns::history_table::size,
                  "Number of steps remembered in this table.")
    ;

  export_algo<apns::proof_number_search>(
    "ProofNumberSearch",
    "The basic variant of the Proof-Number Search algorithm"
  );
  export_algo<apns::depth_first_pns>(
    "DepthFirstPNS",
    "Depth-First variant of the algorithm"
  );

  export_algo<virtual_algo_wrap, apns::search_algo<apns::virtual_algo> >(
    "SearchAlgorithm",
    "Abstract base-class for search algorithms"
  )
    .def("doIterate", pure_virtual(&virtual_algo_wrap::really_do_iterate))
    .add_property("game", &virtual_algo_wrap::game)
    .add_property("hasher", make_function(&virtual_algo_wrap::hasher,
                                          return_internal_reference<>()))
    .add_property("initialHash", &virtual_algo_wrap::initial_hash)
    ;
}
