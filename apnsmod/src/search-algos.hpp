#ifndef SEARCH_ALGOS_HPP
#define SEARCH_ALGOS_HPP

#include "tree.hpp"
#include "hash.hpp"
#include "movement.hpp"

#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/utility.hpp>
#include <boost/timer.hpp>
#include <boost/ref.hpp>
#include <boost/circular_buffer.hpp>

#include <vector>
#include <stack>
#include <cassert>

namespace apns {

/**
 * Did any player win?
 *
 * \param board The game situation.
 * \param player Last player to have made a move.
 * \return If any player has won, return their color; otherwise return nothing.
 */
boost::optional<piece::color_t> winner(board const& board, piece::color_t player);

//! Compare children of a vertex according to the apropriate number.
struct vertex_comparator {
  explicit vertex_comparator(vertex const& parent) :
    number(parent.type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number),
    parent_type(parent.type)
  { }

  bool operator () (vertex const& lhs, vertex const& rhs) {
    return lhs.*number < rhs.*number || (lhs.*number == rhs.*number && lhs.type == parent_type && rhs.type != parent_type);
  }

private:
  vertex::number_t vertex::*  number;
  vertex::e_type              parent_type;
};

//! Compare pointers to children of a vertex according to the apropriate number.
struct vertex_ptr_comparator {
  explicit vertex_ptr_comparator(vertex const* parent) :
    comp(*parent)
  { }

  bool operator () (vertex const* lhs, vertex const* rhs) {
    return comp(*lhs, *rhs);
  }

private:
  vertex_comparator comp;
};

//! Return the best successor of a vertex. Can be used as a traversal policy.
vertex* best_successor(vertex& parent);

//! Return the best successor of a vertex and, if any, the second-best successor.
std::pair<vertex*, vertex*> two_best_successors(vertex& parent);

//! A no-op visitor for board_visitor.
struct null_board_visitor {
  void operator () (vertex const&, board const&) { }
};

//! A stack of boards visited during tree traversal. This doesn't actually store the whole stack but instead only the current
//! board and updates it.
struct board_stack {
  explicit board_stack(apns::board const& initial_board) :
    board(initial_board)
  { }

  apns::board const& top() const {
    return board;
  }

  void push(step const& s) {
    apply(s, board);
  }

  void pop(step const& s) {
    unapply(s, board);
  }

private:
  apns::board board;
};

//! Container for the history of a path. That is, all board states at the point of the beginnings of each player's turn.
struct history_stack {
  struct record {
    zobrist_hasher::hash_t  hash;

    record() { }
    record(zobrist_hasher::hash_t hash) :
      hash(hash)
    { }
  };

  typedef std::vector<record> records_cont;

  history_stack();
  records_cont const& records() const { return current_records; }

  void push(vertex const& vertex, zobrist_hasher::hash_t hash);
  void pop(vertex const& vertex);

private:
  records_cont                current_records;
  std::stack<vertex::e_type>  last;
};

//! History of hashes met during a traversal.
struct hashes_stack {
  typedef std::vector<zobrist_hasher::hash_t> hashes_cont;

  hashes_stack(zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash, piece::color_t attacker) :
    stack(1, initial_hash),
    hasher(&hasher)
  { 
    last_visited.push(last(vertex::type_or, attacker));
  }

  zobrist_hasher::hash_t top() const          { return stack.back(); }
  zobrist_hasher::hash_t opponent_top() const { return hasher->opponent_hash(stack.back()); }

  //! Get a container of the encountered hashes. The top of the stack is the last element in the container; bottom is the first.
  hashes_cont const&     hashes() const { return stack; }

  void push(vertex const& v);
  void pop();

private:
  struct last {
    vertex::e_type    type;
    piece::color_t    player;

    last(vertex::e_type t, piece::color_t p) :
      type(t),
      player(p)
    { }
  };

  hashes_cont           stack;
  zobrist_hasher const* hasher;
  std::stack<last>      last_visited;
};

//! Killer steps database.
class killer_db {
  typedef boost::circular_buffer<step> ply_killers_t;

public:
  typedef ply_killers_t::const_iterator ply_iterator;

  //! Make a killer DB that holds at most killer_count killers for each ply.
  explicit killer_db(std::size_t killer_count) : killer_count(killer_count) { }

  //! Change the maximal number of killers per ply. This will drop all killers stored in this db.
  void resize_plys(std::size_t new_killer_count) {
    if (new_killer_count != killer_count) {
      killers_or.resize(0);
      killers_and.resize(0);
      killer_count = new_killer_count;
    }
  }

  //! Get the maximal number of killers in each ply.
  std::size_t plys_size() const { return killer_count; }

  //! Add a step to the list of killers for given ply and given parent type.
  void add(std::size_t ply, vertex::e_type type, step const& step);

  //! Is the given step a killer for the given ply?
  bool is_killer(std::size_t ply, vertex::e_type type, step const& step) const;

  ply_iterator ply_begin(std::size_t ply, vertex::e_type type) const {
    if (ply < get_plys(type).size()) 
      return get_plys(type)[ply].begin();
    else
      return ply_iterator();
  }

  ply_iterator ply_end(std::size_t ply, vertex::e_type type) const {
    if (ply < get_plys(type).size())
      return get_plys(type)[ply].end();
    else
      return ply_iterator();
  };

private:
  typedef std::vector<ply_killers_t> plys;

  std::size_t killer_count;       //!< How many killers at most per ply.
  plys killers_or;
  plys killers_and;

  plys& get_plys(vertex::e_type type) {
    switch (type) {
    case vertex::type_or:   return killers_or;
    case vertex::type_and:  return killers_and;
    }

    assert(!"Won't get here");
    return killers_or;
  }

  plys const& get_plys(vertex::e_type type) const {
    switch (type) {
    case vertex::type_or:   return killers_or;
    case vertex::type_and:  return killers_and;
    }

    assert(!"Wont' get here");
    return killers_or;
  }
};

//! Update proof- and disproof-numbers of a single vertex.
void update_numbers(vertex& v);

namespace detail {

  //! Expand a leaf.
  //!
  //! \param leaf The leaf to be expanded.
  //! \param state Board state of the leaf.
  //! \param attacker Attacker.
  //! \param trans_tbl Transposition table to use or NULL.
  //! \param hasher The hasher instance used for this tree.
  //! \param leaf_hash Hash value of leaf_state
  //! \param history Game history gathered during the descend to the leaf.
  void expand(vertex& leaf, board_stack& state, piece::color_t attacker,
              transposition_table* trans_tbl, proof_table* proof_tbl, 
              std::size_t ply, killer_db const& killers, hashes_stack& hashes, history_stack const& history);

  //! Cut non-proving (-disproving) children of a vertex.
  std::size_t cut(vertex& parent);

  //! Process a newly vertex in the tree.
  void process_new(vertex& child, vertex const& parent, piece::color_t atatcker, step const& step, 
                   vertex::e_type type, transposition_table* trans_tbl, proof_table* proof_tbl,
                   hashes_stack& hashes, board_stack& state, history_stack const& history);

  //! Simulate a subtree, if possible.
  //!
  //! \returns True if simulation has been successful, and parent has been proved (parent will have children attached, then).
  //! false otherwise.
  bool simulate(vertex& parent, killer_db& killers, std::size_t ply, piece::color_t attacker,
                proof_table* proof_tbl, board_stack& boards, hashes_stack& hashes, history_stack& history);

  //! Convert history as defined by the history_stack into history as defined by proof_table.
  inline history_t proof_history_from_records(history_stack::records_cont const& records) {
    history_t history;
    for (history_stack::records_cont::const_iterator record = records.begin();
         record != records.end(); ++record) {
      history.push_back(record->hash);
    }

    return history;
  }

} // namespace apns::detail

//! A CRTP base class for search algorithms.
template <typename Algo>
class search_algo : private boost::noncopyable {
public:
  boost::shared_ptr<apns::game> get_game() const {
    return game;
  }

  bool finished() const {
    return game->root.proof_number == 0 || game->root.disproof_number == 0;
  }

  //! Make the algorithm use a transposition table.
  void use_trans_tbl(std::size_t size) {
    trans_tbl.reset(new transposition_table(size));
  }

  //! Get the transposition table, if any, used by this algorithm.
  //! \returns Pointer to the transposition table or null if no table is associated with this algorithm.
  transposition_table const* get_trans_tbl() const {
    return trans_tbl.get(); 
  }

  //! Make the algorithm use a proof table.
  void use_proof_tbl(std::size_t size) {
    proof_tbl.reset(new proof_table(size));
  }

  //! Get the proof table, if any, used by this algorithm.
  proof_table const* get_proof_tbl() const {
    return proof_tbl.get();
  }
  
  //! Change the number of killers stored for each ply.
  void set_killer_count(std::size_t new_killer_count) {
    killers.resize_plys(new_killer_count);
  }

  //! Get the max. number of killers stored for each ply.
  std::size_t get_killer_count() const {
    return killers.plys_size();
  }

  //! Set upper GC treshold. If the tree size exceeds this value, GC will be run.
  void set_gc_high(std::size_t new_max) {
    gc_high = new_max;
  }

  //! Get upper GC treshold.
  std::size_t get_gc_high() const {
    return gc_high;
  }

  //! Set lower GC treshold. GC will stop collecting once the tree size falls below this value.
  void set_gc_low(std::size_t new_min) {
    gc_low = new_min;
  }

  //! Get lower GC treshold.
  std::size_t get_gc_low() const {
    return gc_low;
  }

  //! Get the total number of vertices currently held by this algorithm.
  std::size_t get_position_count() const {
    return position_count;
  }

  //! Run the algorithm for ms_how_long milliseconds.
  void run(unsigned ms_how_long) {
    boost::timer timer;

    while (!finished() && (ms_how_long == 0 || timer.elapsed() < ms_how_long / 1000.0))
      iterate();
  }

  void iterate() {
    if (!finished()) {
      static_cast<Algo*>(this)->do_iterate();
    }
  }

protected:
  boost::shared_ptr<apns::game>  game;
  boost::scoped_ptr<transposition_table>  trans_tbl;
  boost::scoped_ptr<proof_table>          proof_tbl;
  zobrist_hasher              hasher;         //!< Hasher to be used during the algorithm.
  zobrist_hasher::hash_t      initial_hash;   //!< Hash corresponding to initial_state.
  killer_db                   killers;
  std::size_t                 position_count;
  std::size_t                 gc_high;
  std::size_t                 gc_low;

  search_algo(boost::shared_ptr<apns::game> const& game, std::size_t position_count = 1) :
    game(game),
    initial_hash(hasher.generate_initial(game->initial_state, game->attacker)),
    killers(2),
    position_count(position_count),
    gc_high(0),
    gc_low(0)
  { }

  template <typename PathIter>
  void process_leaf(PathIter path_begin, PathIter path_end,
                    board_stack& board_stack, hashes_stack& hashes_stack, history_stack& history) {
    if (path_begin != path_end) {
      std::size_t const leaf_ply = std::distance(path_begin, path_end) - 1;
      vertex* leaf = *(path_end - 1);

      if (detail::simulate(*leaf, killers, leaf_ply, game->attacker, proof_tbl.get(), board_stack, hashes_stack, history)) {
        vertex_counter counter;
        traverse(*leaf, backtrack(), boost::ref(counter));
        position_count += counter.count - 1;  // *leaf itself should not be added as it's already in position_count.
      } 
      else {
        expand(*leaf, board_stack, hashes_stack, history);
        position_count += leaf->children_count();
      }

      update_numbers(std::reverse_iterator<PathIter>(path_end), std::reverse_iterator<PathIter>(path_begin),
                     hashes_stack.hashes().rbegin(), history);

      leaf = *(path_end - 1);
      if ((leaf->type == vertex::type_or && leaf->proof_number == 0)
          || (leaf->type == vertex::type_and && leaf->disproof_number == 0))
        killers.add(leaf_ply, leaf->type, *leaf->step);
    }
  }

  void expand(vertex& leaf, board_stack& state, hashes_stack& hashes, history_stack& history) {
    apns::detail::expand(leaf, state, game->attacker, trans_tbl.get(), proof_tbl.get(), hashes.hashes().size() - 1, 
                         killers, hashes, history);
  }

  template <typename HashesRevIter, typename PathRevIter>
  void update_numbers(PathRevIter path_begin, PathRevIter path_end, HashesRevIter hashes_begin, history_stack history) {
    std::size_t ply = std::distance(path_begin, path_end) - 1;
    PathRevIter prev = path_end;
    PathRevIter current = path_begin;

    while (current != path_end) {
      vertex& v = **current;
      apns::update_numbers(v);

      if (trans_tbl && v.proof_number != 0 && v.disproof_number != 0)
        trans_tbl->insert(*hashes_begin, ply, std::make_pair(v.proof_number, v.disproof_number));

      else if (proof_tbl && (v.proof_number == 0 || v.disproof_number == 0)) {
        proof_tbl->insert(
          *hashes_begin, ply,
          proof_entry_t(v.proof_number, v.disproof_number,
                        detail::proof_history_from_records(history.records()))
        );
      }

      if (current + 1 != path_end) {
        vertex& parent = **(current + 1);

        if ((parent.type == vertex::type_or && v.proof_number == 0)
            || (parent.type == vertex::type_and && v.disproof_number == 0))
          killers.add(ply, parent.type, *v.step);
      }

      if (prev != path_end)
        *prev = &*resort_children(**current, (**current).iter_from_ptr(*prev), vertex_comparator(**current));

      history.pop(**current);
      prev = current;
      ++current;
      ++hashes_begin;
      --ply;
    }
  }

  void garbage_collect() {
    if (gc_high > 0 && position_count > gc_high) {
      // Traverse along the best path cutting children in worst-first manner. Note that gc_traversal never traverses the best
      // vertex itself, so we won't cut the best path.
      
      vertex* current = &game->root;
      while (position_count > gc_low && current != 0) {
        traverse_postorder(*current, gc_traversal(), cutter(position_count), gc_stop(position_count, gc_low));

        if (current->children_count() > 0)
          current = &*current->children_begin();
        else
          current = 0;
      }
    }
  }

private:
  //! Traversal policy that backtracks the tree in a post-order, worst-first manner and leaves out the best N children of each
  //! subtree. It never traverses the root vertex.
  struct gc_traversal {
    static std::size_t const LEAVE_OUT = 2; //!< How many best vertices are to be left out in each ply.

    typedef std::stack<std::pair<apns::vertex::reverse_children_iterator, apns::vertex::reverse_children_iterator> > stack_t;

  public:
    vertex* operator () (apns::vertex& v) {
      using namespace apns;

      if (stack.empty())
        return inc(recurse(v));

      if (stack.top().first != stack.top().second)
        return inc(recurse(*stack.top().first));
      else {
        while (!stack.empty() && stack.top().first == stack.top().second)
          stack.pop();

        if (!stack.empty())
          return &*(stack.top().first++);
        else
          return 0;
      }
    }

  private:
    stack_t stack;

    //! If an iterator is given, increment it and return the original. If boost::none is given, return a singular iterator.
    vertex* inc(boost::optional<vertex::reverse_children_iterator&> v) {
      if (v)
        return &*((*v)++);
      else
        return 0;
    }

  //! Recurse to subtree.
  boost::optional<vertex::reverse_children_iterator&> recurse(vertex& from) {
    using namespace apns;

    vertex* current = &from;
    while (current->children_count() > LEAVE_OUT) {
      vertex::reverse_children_iterator end = current->children_rend();
      std::advance(end, -LEAVE_OUT);
      stack.push(std::make_pair(current->children_rbegin(), end));
      current = &*current->children_rbegin();
    }

    if (!stack.empty())
      return stack.top().first;
    else
      return boost::none;
  }
};
  //! Visitor that cuts children of the visited vertex and decreases the position count.
  struct cutter {
    std::size_t* count;  //!< Pointer to the total position count in the tree.

    explicit cutter(std::size_t& count) : count(&count) { }
    void operator () (vertex& v) { *count -= detail::cut(v); }
  };

  //! Traversal stop policy that stops the algorithm when the position count drops below the given level.
  struct gc_stop {
    gc_stop(std::size_t& count, std::size_t low) :
      count(&count),
      low(low)
    { }

    bool operator () (vertex&) {
      return *count <= low;
    }

  private:
    std::size_t* count;
    std::size_t  low;
  };
};

//! The basic variant of the Proof-Number Search algorithm.
class proof_number_search : public search_algo<proof_number_search> {
public:
  //! Make an algorithm instance for the given game instance. 
  //!
  //! \param position_count Number of positions in the passed-in game.
  explicit proof_number_search(boost::shared_ptr<apns::game> const& game, std::size_t position_count = 1) :
    search_algo(game, position_count)
  { }

  //! Perform an iteration of the algorithm.
  void do_iterate();
};

//! Depth-first variant of the Proof-Number Search algorithm.
struct depth_first_pns : public search_algo<depth_first_pns> {
  explicit depth_first_pns(boost::shared_ptr<apns::game> const& game, std::size_t position_count = 1) :
    search_algo(game, position_count),
    path(1, &game->root),
    limits(1, limits_t(vertex::infty, vertex::infty)),
    hashes(hasher, initial_hash, game->attacker),
    boards(game->initial_state)
  { }

  void do_iterate();

  void do_set_gc_high(std::size_t new_max) {
    gc_high = new_max;
  }

  std::size_t do_get_gc_high() const {
    return gc_high;
  }
  
  void do_set_gc_low(std::size_t new_min) {
    gc_low = new_min;
  }

  std::size_t do_get_gc_low() const {
    return gc_low;
  }
  
private:
  struct limits_t {
    vertex::number_t pn_limit;
    vertex::number_t dn_limit;

    limits_t() { }
    limits_t(vertex::number_t pn, vertex::number_t dn) : pn_limit(pn), dn_limit(dn) { }
  };

  typedef std::vector<vertex*>  path_cont;
  typedef std::vector<limits_t> limits_cont;

  path_cont       path;
  limits_cont     limits;
  history_stack   history;
  hashes_stack    hashes;
  board_stack     boards;

  limits_t make_limits(vertex& v, vertex& parent, boost::optional<vertex&> second_best, limits_t parent_limits);
};

} // namespace apns

#endif

