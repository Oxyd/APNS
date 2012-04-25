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

struct virtual_algo_wrap : public apns::virtual_algo, boost::python::wrapper<apns::virtual_algo> {
  virtual_algo_wrap(boost::shared_ptr<apns::game> const& game, unsigned position_count = 1) :
    virtual_algo(game, position_count)
  { }

  virtual_algo_wrap(PyObject*, boost::shared_ptr<apns::game> const& game, unsigned position_count = 1) :
    virtual_algo(game, position_count)
    { }

  virtual void really_do_iterate() {
    this->get_override("doIterate")();
  }

  boost::shared_ptr<apns::game> game() { return game_; }
  apns::zobrist_hasher& hasher() { return hasher_; }
  apns::zobrist_hasher::hash_t initial_hash() { return initial_hash_; }
  apns::search_tree& tree() { return tree_; }
};

template <typename Algo, typename Base>
boost::python::class_<Algo, boost::python::bases<Base>, boost::noncopyable>
export_algo(char const* name, char const* description) {
  using namespace boost::python;

  std::string base_name = std::string("_") + typeid(Base).name();
  class_<Base, boost::noncopyable>(base_name.c_str(), no_init)
    .add_property("game", &Base::get_game,
                  "The Game object associated with this algorithm")
    .add_property("finished", &Base::finished,
                  "Has the algorithm finished?")
    .def("iterate", &Base::iterate,
         "algo.iterate() -> None\n\nPerform a single iteration of the algorithm")
    .def("run", &Base::run,
         "algo.run(msHowLong) -> None\n\nRun the algorithm for msHowLong milliseconds. If msHowLong is 0, the algorithm will run"
         "until it finishes.")
    .def("useTransTbl", &Base::use_trans_tbl,
         "algo.useTransTbl(size) -> None\n\n"
         "Make this algorithm use a transposition table of given size.")
    .def("useProofTbl", &Base::use_proof_tbl,
         "algo.useProofTbl(size) -> None\n\n"
         "Make this algorithm use a proof table of given size.")
    
    .add_property("transpositionTable", 
                  make_function(&Base::get_trans_tbl, return_internal_reference<>()),
                  "The TranspositionTable instance associated with this algorithm or None.")
    .add_property("proofTable",
                  make_function(&Base::get_proof_tbl, return_internal_reference<>()),
                  "The ProofTable instance associated with this algorithm or None.")
    .add_property("positionCount",
                  &Base::get_position_count,
                  "Total number of vertices currently held by this algorithm")
    .add_property("moveCacheSize",
                  static_cast<std::size_t (Base::*)() const>(&Base::move_cache_size),
                  static_cast<void (Base::*)(std::size_t)>(&Base::move_cache_size),
                  "Size of the moves cache.")
    .add_property("moveCacheHits",
                  &Base::move_cache_hits,
                  "Number of hits in the move cache.")
    .add_property("moveCacheMisses",
                  &Base::move_cache_misses,
                  "Number of misses in the move cache.")
    ;

  return class_<Algo, bases<Base>, boost::noncopyable>(name, description,
                                                       init<boost::shared_ptr<apns::game> const&>())
    .def(init<boost::shared_ptr<apns::game> const&, unsigned>())
    ;
}

template <typename Algo>
boost::python::class_<Algo, boost::python::bases<apns::search_algo<Algo> >, boost::noncopyable>
export_algo(char const* name, char const* description) {
  return export_algo<Algo, apns::search_algo<Algo> >(name, description);
}

} // anonymous namespace

//! Export types and functions declared in search-algos.hpp.
void export_search_algos() {
  using namespace boost::python;

  def("bestSuccessor", static_cast<apns::vertex* (*)(apns::vertex&)>(&apns::best_successor),
      return_internal_reference<>(),
      "bestSuccessor(Vertex) -> Vertex\n\nReturns the best of all vertex's successors, or None if the given vertex is a leaf.");

  class_<apns::search_tree, boost::noncopyable>("SearchTree",
                                                "Performs operations on the search tree, including various heuristics.",
                                                init<apns::vertex*, apns::piece::color_t, std::size_t, apns::zobrist_hasher const&,
                                                     apns::zobrist_hasher::hash_t, apns::board const&>())
    .add_property("current", make_function(&apns::search_tree::current, return_internal_reference<>()),
                  "The currently-selected vertex")
    .def("selectChild", static_cast<void (apns::search_tree::*)(apns::vertex const*)>(&apns::search_tree::select_child),
         "t.selectChild(Vertex)\n\nMake a child of the current vertex the new current vertex. Behaviour is undefined if "
         "the given vertex is not a child of the current one.")
    .def("selectParent", &apns::search_tree::select_parent,
         "t.selectParent() -> None\n\nSelect the parent of the current vertex. The current vertex must not be the root.")
    .add_property("parent", make_function(&apns::search_tree::parent, return_internal_reference<>()),
                  "The parent of the currently-selected vertex, which must not be the root.")
    .def("selectRoot", &apns::search_tree::select_root,
         "t.selectRoot() -> None\n\nSelect the root of the tree.")
    .add_property("atRoot", &apns::search_tree::at_root, "Is the currently-selected vertex the root?")
    .add_property("selectionDepth", &apns::search_tree::selection_depth, "How deep is the current selection?")
    .def("expand", &apns::search_tree::expand,
         "t.expand() -> None\n\nExpand the currently-selected vertex, which must be a leaf.")
    .def("evaluate", &apns::search_tree::evaluate,
         "t.evaluate() -> None\n\nEvaluate the current vertex")
    .def("evaluateChildren", &apns::search_tree::evaluate_children,
         "t.evaluateChildren() -> None\n\nEvaluate all children of the current selection.")
    .def("evaluateCached", &apns::search_tree::evaluate_cached,
         "t.evaluateCached() -> Bool\n\nAttempt to look up current vertex's numbers in the tables. Return True on success.")
    .def("cutChildren", &apns::search_tree::cut_children,
         "t.cutChildren() -> None\n\nRemove all children of the currently-selected vertex.")
    .def("reduce", &apns::search_tree::reduce,
         "t.reduce() -> None\n\nAssuming the current vertex only contains leaves, reduce the move by removing duplicate "
         "paths for the same end position.")
    .def("updatePath", &apns::search_tree::update_path,
         "t.updatePath() -> None\n\nUpdate proof- and disproof number along the current path.")
    .def("useTransTbl", &apns::search_tree::use_trans_tbl,
         "t.useTransTbl(size) -> None\n\nMake this tree use a transposition table of given size")
    .def("useProofTbl", &apns::search_tree::use_proof_tbl,
         "t.useProofTbl(size) -> None\n\nMake this tree use a proof table of given size")
    .def("cacheMoves", &apns::search_tree::cache_moves,
         "t.cacheMoves(count) -> None\n\nSet the size of the move cache")
    .add_property("transTbl", make_function(&apns::search_tree::trans_tbl, return_internal_reference<>()),
                  "The TranspositionTable instance currently in use")
    .add_property("proofTbl", make_function(&apns::search_tree::proof_tbl, return_internal_reference<>()),
                  "The ProofTable instance currently in use")
    .add_property("cachedMoves", &apns::search_tree::cached_moves, "How big is the moves cache?")
    .add_property("size", &apns::search_tree::size, "Number of vertices in the tree")
    ;

  def("selectBest", &apns::select_best,
      "selectBest(SearchTree) -> None\n\nSelect the best successor of the current vertex in the tree");

  export_algo<apns::proof_number_search>("ProofNumberSearch", "The basic variant of the Proof-Number Search algorithm");
  export_algo<apns::depth_first_pns>("DepthFirstPNS", "Depth-First variant of the algorithm");

  export_algo<virtual_algo_wrap, apns::search_algo<apns::virtual_algo> >("SearchAlgorithm", "Abstract base-class for search algorithms")
    .def("doIterate", pure_virtual(&virtual_algo_wrap::really_do_iterate))
    .add_property("game", &virtual_algo_wrap::game)
    .add_property("hasher", make_function(&virtual_algo_wrap::hasher, return_internal_reference<>()))
    .add_property("initialHash", &virtual_algo_wrap::initial_hash)
    .add_property("tree", make_function(&virtual_algo_wrap::tree, return_internal_reference<>()))
    ;
}
