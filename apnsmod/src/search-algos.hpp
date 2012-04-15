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
    number_(parent.type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number),
    parent_type_(parent.type)
  { }

  bool operator () (vertex const& lhs, vertex const& rhs) {
    return lhs.*number_ < rhs.*number_ || (lhs.*number_ == rhs.*number_ && lhs.type == parent_type_ && rhs.type != parent_type_);
  }

private:
  vertex::number_t vertex::*  number_;
  vertex::e_type              parent_type_;
};

//! Compare pointers to children of a vertex according to the apropriate number.
struct vertex_ptr_comparator {
  explicit vertex_ptr_comparator(vertex const* parent) :
    comp_(*parent)
  { }

  bool operator () (vertex const* lhs, vertex const* rhs) {
    return comp_(*lhs, *rhs);
  }

private:
  vertex_comparator comp_;
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
    board_(initial_board)
  { }

  apns::board const& top() const {
    return board_;
  }

  void push(step const& s) {
    apply(s, board_);
  }

  void pop(step const& s) {
    unapply(s, board_);
  }

private:
  apns::board board_;
};

//! Container for the history of a path. That is, all board states at the point of the beginnings of each player's turn.
struct history_stack {
  struct record {
    zobrist_hasher::hash_t  hash;

    record() { }
    explicit record(zobrist_hasher::hash_t hash) :
      hash(hash)
    { }
  };

  typedef std::vector<record> records_cont;

  history_stack();
  records_cont const& records() const { return current_records_; }

  void push(vertex const& vertex, zobrist_hasher::hash_t hash);
  void pop(vertex const& vertex);

private:
  records_cont                current_records_;
  std::stack<vertex::e_type>  last_;
};

//! History of hashes met during a traversal.
struct hashes_stack {
  typedef std::vector<zobrist_hasher::hash_t> hashes_cont;

  hashes_stack(zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash, piece::color_t attacker) :
    stack_(1, initial_hash),
    hasher_(&hasher)
  { 
    last_visited_.push(last(vertex::type_or, attacker));
  }

  zobrist_hasher::hash_t top() const          { return stack_.back(); }
  zobrist_hasher::hash_t opponent_top() const { return hasher_->opponent_hash(stack_.back()); }

  //! Get a container of the encountered hashes. The top of the stack is the last element in the container; bottom is the first.
  hashes_cont const&     hashes() const { return stack_; }

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

  hashes_cont           stack_;
  zobrist_hasher const* hasher_;
  std::stack<last>      last_visited_;
};

//! Killer steps database.
class killer_db {
  typedef boost::circular_buffer<step> ply_killers_t;

public:
  typedef ply_killers_t::const_iterator ply_iterator;

  //! Make a killer DB that holds at most killer_count killers for each ply.
  explicit killer_db(std::size_t killer_count) : killer_count_(killer_count) { }

  //! Change the maximal number of killers per ply. This will drop all killers stored in this db.
  void resize_plys(std::size_t new_killer_count) {
    if (new_killer_count != killer_count_) {
      killers_or_.resize(0);
      killers_and_.resize(0);
      killer_count_ = new_killer_count;
    }
  }

  //! Get the maximal number of killers in each ply.
  std::size_t plys_size() const { return killer_count_; }

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

  std::size_t killer_count_;       //!< How many killers at most per ply.
  plys killers_or_;
  plys killers_and_;

  plys& get_plys(vertex::e_type type) {
    switch (type) {
    case vertex::type_or:   return killers_or_;
    case vertex::type_and:  return killers_and_;
    }

    assert(!"Won't get here");
    return killers_or_;
  }

  plys const& get_plys(vertex::e_type type) const {
    switch (type) {
    case vertex::type_or:   return killers_or_;
    case vertex::type_and:  return killers_and_;
    }

    assert(!"Wont' get here");
    return killers_or_;
  }
};

//! A move path is a path from the vertex that begins a player's turn to the vertex that ends it. That means that the last
//! vertex's type is always different from the first one's. Also that all but the last vertex share the same type.
//!
//! For simplicity, this is an array of 4 vertices (the maximum path len). The first element is the last vertex. Null pointers
//! are used when the path is shorter than 4 vertices.
typedef boost::array<vertex*, 4> move_path;

//! A move terminal is the vertex that ends a player's turn. This is a mapping from such terminals' hashes into their paths.
typedef boost::unordered_map<zobrist_hasher::hash_t, move_path> terminal_paths;

//! A move tree is a subtree of the whole search tree rooted at the vertex that begins a player's turn and containing the
//! possible step paths from this root to terminal states.
struct move_tree {
  zobrist_hasher::hash_t            root_hash;
  boost::shared_ptr<terminal_paths> paths;
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

//! Manages the various information that are to be kept during descent through the tree.
class search_stack {
public:
  typedef std::vector<vertex*>                path_sequence;
  typedef std::vector<zobrist_hasher::hash_t> hashes_sequence;
  typedef std::vector<zobrist_hasher::hash_t> history_sequence;

  //! Create a stack and push the root vertex on it.
  search_stack(zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash,
               vertex* root, piece::color_t attacker, board const& initial_state);

  void push(vertex* v); //!< Push a successor of the currently-last vertex.
  void pop();           //!< Pop a vertex. The stack must not be at root before this call.

  //! Is the last vertex the root?
  bool at_root() const { return path_.size() == 1; }

  path_sequence const&    path() const    { return path_; }
  hashes_sequence const&  hashes() const  { return hashes_; }
  history_sequence const& history() const { return history_; }
  board const&            state() const   { return state_; }

  vertex*                 path_top() const    { return path_.back(); }
  zobrist_hasher::hash_t  hashes_top() const  { return hashes_.back(); }
  zobrist_hasher::hash_t  history_top() const { return history_.back(); }

private:
  path_sequence         path_;
  hashes_sequence       hashes_;
  history_sequence      history_;
  board                 state_;
  zobrist_hasher const* hasher_;
  piece::color_t        attacker_;
};

//! Allows temporary extensions to the search_stack. Whenever you create a checkpoint, you may push further vertices to the
//! search_stack. When the checkpoint is destroyed, the stack is restored to the original state. Do note that you may *not*
//! pop any vertices from the stack other than those that were pushed after this checkpoint was created.
//!
//! The stack must not be destroyed during the lifetime of the checkpoint.
struct search_stack_checkpoint {
  explicit search_stack_checkpoint(search_stack& stack) :
      stack_(&stack),
      original_length_(stack.path().size())
  { }

  ~search_stack_checkpoint() { revert(); }

  //! Explicitely revert the stack to the saved state.
  void revert();

private:
  search_stack* stack_;
  std::size_t   original_length_;
};

//! A CRTP base class for search algorithms.
template <typename Algo>
class search_algo : private boost::noncopyable {
public:
  boost::shared_ptr<apns::game> get_game() const {
    return game_;
  }

  bool finished() const {
    return game_->root.proof_number == 0 || game_->root.disproof_number == 0;
  }

  //! Make the algorithm use a transposition table.
  void use_trans_tbl(std::size_t size) {
    trans_tbl_.reset(new transposition_table(size));
  }

  //! Get the transposition table, if any, used by this algorithm.
  //! \returns Pointer to the transposition table or null if no table is associated with this algorithm.
  transposition_table const* get_trans_tbl() const {
    return trans_tbl_.get(); 
  }

  //! Make the algorithm use a proof table.
  void use_proof_tbl(std::size_t size) {
    proof_tbl_.reset(new proof_table(size));
  }

  //! Get the proof table, if any, used by this algorithm.
  proof_table const* get_proof_tbl() const {
    return proof_tbl_.get();
  }
  
  //! Change the number of killers stored for each ply.
  void set_killer_count(std::size_t new_killer_count) {
    killers_.resize_plys(new_killer_count);
  }

  //! Get the max. number of killers stored for each ply.
  std::size_t get_killer_count() const {
    return killers_.plys_size();
  }

  //! Set upper GC treshold. If the tree size exceeds this value, GC will be run.
  void set_gc_high(std::size_t) {
  }

  //! Get upper GC treshold.
  std::size_t get_gc_high() const {
    return gc_high_;
  }

  //! Set lower GC treshold. GC will stop collecting once the tree size falls below this value.
  void set_gc_low(std::size_t) {
  }

  //! Get lower GC treshold.
  std::size_t get_gc_low() const {
    return gc_low_;
  }

  //! Get the total number of vertices currently held by this algorithm.
  std::size_t get_position_count() const {
    return position_count_;
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
  boost::shared_ptr<apns::game>  game_;
  boost::scoped_ptr<transposition_table>  trans_tbl_;
  boost::scoped_ptr<proof_table>          proof_tbl_;
  zobrist_hasher              hasher_;         //!< Hasher to be used during the algorithm.
  zobrist_hasher::hash_t      initial_hash_;   //!< Hash corresponding to initial_state.
  killer_db                   killers_;
  std::size_t                 position_count_;
  std::size_t                 gc_high_;
  std::size_t                 gc_low_;

  search_algo(boost::shared_ptr<apns::game> const& game, std::size_t position_count = 1) :
    game_(game),
    initial_hash_(hasher_.generate_initial(game->initial_state, game->attacker)),
    killers_(2),
    position_count_(position_count),
    gc_high_(0),
    gc_low_(0)
  { }

  void expand_leaf(search_stack& stack) {

  }

  template <typename PathIter>
  void process_leaf(PathIter path_begin, PathIter path_end,
                    board_stack& board_stack, hashes_stack& hashes_stack, history_stack& history) {
    if (path_begin != path_end) {
      std::size_t const leaf_ply = std::distance(path_begin, path_end) - 1;
      vertex* leaf = *(path_end - 1);

      if (detail::simulate(*leaf, killers_, leaf_ply, game_->attacker, proof_tbl_.get(), board_stack, hashes_stack, history)) {
        vertex_counter counter;
        traverse(*leaf, backtrack(), boost::ref(counter));
        position_count_ += counter.count - 1;  // *leaf itself should not be added as it's already in position_count.
      } 
      else {
        expand(*leaf, board_stack, hashes_stack, history);
        position_count_ += leaf->children_count();
      }

      update_numbers(std::reverse_iterator<PathIter>(path_end), std::reverse_iterator<PathIter>(path_begin),
                     hashes_stack.hashes().rbegin(), history);

      leaf = *(path_end - 1);
      if ((leaf->type == vertex::type_or && leaf->proof_number == 0)
          || (leaf->type == vertex::type_and && leaf->disproof_number == 0))
        killers_.add(leaf_ply, leaf->type, *leaf->step);
    }
  }

  void expand(vertex& leaf, board_stack& state, hashes_stack& hashes, history_stack& history) {
    apns::detail::expand(leaf, state, game_->attacker, trans_tbl_.get(), proof_tbl_.get(), hashes.hashes().size() - 1, 
                         killers_, hashes, history);
  }

  template <typename HashesRevIter, typename PathRevIter>
  void update_numbers(PathRevIter path_begin, PathRevIter path_end, HashesRevIter hashes_begin, history_stack history) {
    std::size_t ply = std::distance(path_begin, path_end) - 1;
    PathRevIter prev = path_end;
    PathRevIter current = path_begin;

    while (current != path_end) {
      vertex& v = **current;
      apns::update_numbers(v);

      if (trans_tbl_ && v.proof_number != 0 && v.disproof_number != 0)
        trans_tbl_->insert(*hashes_begin, ply, std::make_pair(v.proof_number, v.disproof_number));

      else if (proof_tbl_ && (v.proof_number == 0 || v.disproof_number == 0)) {
        proof_tbl_->insert(
          *hashes_begin, ply,
          proof_entry_t(v.proof_number, v.disproof_number,
                        detail::proof_history_from_records(history.records()))
        );
      }

      if (current + 1 != path_end) {
        vertex& parent = **(current + 1);

        if ((parent.type == vertex::type_or && v.proof_number == 0)
            || (parent.type == vertex::type_and && v.disproof_number == 0))
          killers_.add(ply, parent.type, *v.step);
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

  //! If the number of vertices is greater than gc_high, cut vertices until the count drops below gc_low. An effort is made
  //! to cut the worst and deepest vertices first.
  void garbage_collect() {
    if (gc_high_ > 0 && position_count_ > gc_high_) {
      // Traverse along the best path cutting children in worst-first manner. Note that gc_traversal never traverses the best
      // vertex itself, so we won't cut the best path.
      
      vertex* current = &game_->root;
      while (position_count_ > gc_low_ && current != 0) {
        traverse_postorder(*current, gc_traversal(), cutter(position_count_), gc_stop(position_count_, gc_low_));

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
      count_(&count),
      low_(low)
    { }

    bool operator () (vertex&) {
      return *count_ <= low_;
    }

  private:
    std::size_t* count_;
    std::size_t  low_;
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
    path_(1, &game->root),
    limits_(1, limits_t(vertex::infty, vertex::infty)),
    hashes_(hasher_, initial_hash_, game_->attacker),
    boards_(game_->initial_state)
  { }

  void do_iterate();

  void do_set_gc_high(std::size_t new_max) {
    gc_high_ = new_max;
  }

  std::size_t do_get_gc_high() const {
    return gc_high_;
  }
  
  void do_set_gc_low(std::size_t new_min) {
    gc_low_ = new_min;
  }

  std::size_t do_get_gc_low() const {
    return gc_low_;
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

  path_cont       path_;
  limits_cont     limits_;
  history_stack   history_;
  hashes_stack    hashes_;
  board_stack     boards_;

  limits_t make_limits(vertex& v, vertex& parent, boost::optional<vertex&> second_best, limits_t parent_limits);
};

} // namespace apns

#endif

