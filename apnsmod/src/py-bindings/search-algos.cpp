#include "search-algos.hpp"
#include "py-utils.hpp"

#include <boost/python.hpp>

namespace {

struct proof_number_search_wrap : public apns::proof_number_search {
  proof_number_search_wrap(PyObject*, boost::shared_ptr<apns::game> const& game)
    : proof_number_search(game) { }
};

struct depth_first_pns_wrap : public apns::depth_first_pns {
  depth_first_pns_wrap(PyObject*, boost::shared_ptr<apns::game> const& game)
    : depth_first_pns(game) { }
};

struct virtual_algo_wrap : public apns::virtual_algo, boost::python::wrapper<apns::virtual_algo> {
  virtual_algo_wrap(boost::shared_ptr<apns::game> const& game)
    : virtual_algo(game) { }

  virtual_algo_wrap(PyObject*, boost::shared_ptr<apns::game> const& game)
    : virtual_algo(game) { }

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
    
    .add_property("transpositionTable", 
                  &Base::get_trans_tbl, &Base::use_trans_tbl,
                  "The TranspositionTable instance associated with this "
                  "algorithm or None.")
    .add_property("proofTable",
                  &Base::get_proof_tbl, &Base::use_proof_tbl,
                  "The ProofTable instance associated with this algorithm or "
                  "None.")
    .add_property("killerDB",
                  &Base::get_killer_db, &Base::use_killer_db,
                  "The KillerDB instance associated with this algorithm or "
                  "None.")
    .add_property("positionCount",
                  &Base::get_position_count,
                  "Total number of vertices currently held by this algorithm")
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
  ) .def(init<boost::shared_ptr<apns::game> const&>())
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

//! An iterator over the sequence of killers on a ply.
struct killer_ply_iterator {
  killer_ply_iterator(apns::killer_db::level_iterator begin,
                      apns::killer_db::level_iterator end)
    : current_(begin)
    , end_(end)
  { }

  killer_ply_iterator iter() const { return *this; }

  boost::python::object next() {
    using namespace boost::python;

    if (current_ != end_) {
      return object(*current_++);
    } else {
      PyErr_SetNone(PyExc_StopIteration);
      throw_error_already_set();
      return object();
    }
  }

private:
  apns::killer_db::level_iterator current_;
  apns::killer_db::level_iterator end_;
};

killer_ply_iterator killer_db_killers(apns::killer_db const& db,
                                      std::size_t ply) {
  return killer_ply_iterator(db.level_begin(ply), db.level_end(ply));
}

} // anonymous namespace

//! Export types and functions declared in search-algos.hpp.
void export_search_algos() {
  using namespace boost::python;

  to_python_converter<boost::optional<apns::piece::color_t>,
                      optional_to_T<apns::piece::color_t> >();

  def("bestSuccessor",
      static_cast<apns::vertex* (*)(apns::vertex&)>(&apns::best_successor),
      return_internal_reference<>(),
      "bestSuccessor(Vertex) -> Vertex\n\n"
      "Returns the best of all vertex's successors, or None if the given "
      "vertex is a leaf.");

  def("vertexPlayer",
      &apns::vertex_player,
      "vertexPlayer(Vertex, attacker) -> Color\n\n"
      "Get the player on turn in given vertex.");

  def("winner", &apns::winner,
      "winner(Board, player) -> Color\n\n"
      "Determine whether a player is a winner in the given position, assuming "
      "that the given player has just made a move and their opponent is on turn.");

  class_<killer_ply_iterator>("KillerPlyIterator", no_init)
    .def("__iter__", &killer_ply_iterator::iter)
    .def("__next__", &killer_ply_iterator::next)
    ;

  class_<apns::killer_db>("KillerDB",
                          "Remembers each player's killer steps for a "
                          "ply or level",
                          init<std::size_t>())
    .add_property("plysSize",
                  &apns::killer_db::levels_size, &apns::killer_db::resize_plys,
                  "How many killers are remembered for each ply")
    .add_property("totalSize",
                  &apns::killer_db::total_size,
                  "Total number of elements in this DB.")
    .def("add", &apns::killer_db::add,
         "k.add(ply, type, step) -> None\n\n"
         "Add a killer to the db.")
    .def("isKiller", &apns::killer_db::is_killer,
         "k.isKiller(ply, type, step) -> Bool\n\n"
         "Check whether given step is a killer on given ply.")
    .def("killers", &killer_db_killers,
         "k.killers(ply, type) -> [Step]\n\n"
         "Get the killers recorded for given ply and type.")
    ;

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
