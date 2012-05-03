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
#include <boost/lambda/lambda.hpp>
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>

#include <vector>
#include <stack>
#include <cassert>
#include <utility>
#include <ostream>

namespace apns {

class log_sink;

/**
 * Did any player win?
 *
 * \param board The game situation.
 * \param player Last player to have made a move.
 * \return If any player has won, return their color; otherwise return nothing.
 */
boost::optional<piece::color_t> 
winner(board const& board, piece::color_t player);

//! Compare children of a vertex according to the apropriate number.
struct vertex_comparator {
  //! Make a comparator to compare children of parent.
  //! \param best_first When true, children are sorted in best-first manner;
  //!   when false, they are sorted in worst-first manner instead.
  explicit vertex_comparator(vertex const& parent, bool best_first = true) { 
    if (best_first) {
      number_ = parent.type == vertex::type_or ?
        &vertex::proof_number : &vertex::disproof_number;
      parent_type_ = parent.type;
    } else {
      number_ = parent.type == vertex::type_and ?
        &vertex::proof_number : &vertex::disproof_number;
      parent_type_ = opposite_type(parent.type);
    }
  }

  bool operator () (vertex const& lhs, vertex const& rhs) {
    // Performance note: It currently appears to be faster with this sort of
    // lexicographial sorting that puts same-type children before other-type
    // ones.
    
    return lhs.*number_ < rhs.*number_ || 
            (lhs.*number_ == rhs.*number_ &&
             lhs.type == parent_type_ &&
             rhs.type != parent_type_);
  }

private:
  vertex::number_t vertex::*  number_;
  vertex::e_type              parent_type_;
};

//! Compare pointers to children of a vertex according to the apropriate 
//! number.
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
//! If there are multiple "as good" children, this guarantees to select the
//! first one of these. This is to preserve any relative heuristic order
//! created on these chilren.
vertex* best_successor(vertex& parent);
vertex const* best_successor(vertex const& parent);

//! Return the best successor of a vertex and, if any, the second-best
//! successor.
std::pair<vertex*, vertex*>
two_best_successors(vertex& parent);

//! Killer steps database.
class killer_db {
  typedef boost::circular_buffer<step> ply_killers_t;

public:
  typedef ply_killers_t::const_iterator ply_iterator;

  //! Make a killer DB that holds at most killer_count killers for each ply.
  explicit killer_db(std::size_t killer_count) 
    : killer_count_(killer_count) 
  { }

  //! Change the maximal number of killers per ply. This will drop all killers 
  //! stored in this db.
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

    assert(!"Won't get here");
    return killers_or_;
  }
};

//! History table serves for step-ordering within a vertex.
class history_table {
public:
  //! Advice the table that step caused a cut-off at depth.
  void insert(step const& step, std::size_t depth);

  //! Sort the children of a vertex according to this table.
  void sort(vertex& v) const;

  //! Count of unique steps that were inserted into the table.
  std::size_t size() const { return table_.size(); }

private:
  typedef boost::unordered_map<step, boost::uint64_t> table_t;

  struct compare {
    explicit compare(table_t& t) 
      : table_(&t) 
    { }

    bool operator () (vertex const& lhs, vertex const& rhs);

  private:
    table_t* table_;
  };

  boost::unordered_map<step, boost::uint64_t> table_;
};

//! Update proof- and disproof-numbers of a single vertex.
void update_numbers(vertex& v);

//! Manages the various information that are to be kept during descent through 
//! the tree.
class search_stack {
public:
  typedef std::vector<vertex*>                path_sequence;
  typedef std::vector<zobrist_hasher::hash_t> hashes_sequence;
  typedef std::vector<zobrist_hasher::hash_t> history_sequence;

  //! Create a stack and push the root vertex on it.
  search_stack(
    zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash,
    vertex* root, piece::color_t attacker, board const& initial_state
  );

  //! Push a successor of the currently top vertex. The behaviour is undefined
  //! if v is not a child of the current top vertex.
  void push(vertex* child); 

  //! Pop a vertex. The stack must not be at root before this call.
  void pop();           

  //! Pop this stack until it is at_root.
  void reset_to_root();

  //! Is the last vertex the root?
  bool at_root() const { return path_.size() == 1; }

  //! How long is the current path?
  std::size_t size() const { return path_.size(); }

  path_sequence const&    path() const    { return path_; }
  hashes_sequence const&  hashes() const  { return hashes_; }
  history_sequence const& history() const { return history_; }
  board const&            state() const;

  vertex*                 path_top() const    { return path_.back(); }
  zobrist_hasher::hash_t  hashes_top() const  { return hashes_.back(); }
  zobrist_hasher::hash_t  history_top() const { return history_.back(); }

  zobrist_hasher const& hasher() const { return *hasher_; }

private:
  // Implementation note: state_ is lazy -- the step sequence is only applied 
  // to it when state() is requested. The state_pos_ variable is an index into
  // path_ that specifies the last vertex whose step was applied to state_.

  path_sequence                     path_;
  hashes_sequence                   hashes_;
  history_sequence                  history_;
  mutable board                     state_;
  mutable path_sequence::size_type  state_pos_;
  zobrist_hasher const*             hasher_;
  piece::color_t                    attacker_;
};

//! Stream the current path into a stream.
std::ostream& operator << (std::ostream& out, search_stack const& stack);

//! Format a path into an ostream.
std::ostream& format_path(std::ostream& out,
                          search_stack::path_sequence::const_iterator begin,
                          search_stack::path_sequence::const_iterator end);

//! Allows temporary extensions to the search_stack. Whenever you create a
//! checkpoint, you may push further vertices to the search_stack. When the
//! checkpoint is destroyed, the stack is restored to the original state. Do 
//! note that you may *not* pop any vertices from the stack other than those 
//! that were pushed after this checkpoint was created.
//!
//! The stack must not be destroyed during the lifetime of the checkpoint.
struct search_stack_checkpoint {
  explicit search_stack_checkpoint(search_stack& stack) :
      stack_(&stack),
      original_top_(stack.path_top())
  { }

  ~search_stack_checkpoint() { revert(); }

  //! Explicitely revert the stack to the saved state.
  void revert();

private:
  search_stack* stack_;
  vertex*       original_top_;
};

/** A move path is a path from the vertex that begins a player's turn to the
 * vertex that ends it. That means that the last vertex's type is always
 * different from the first one's. Also that all but the last vertex share the
 * same type.
 *
 * For simplicity, this is an array of 4 vertices (the maximum path len). The
 * first element is the move-root vertex. Null pointers are used when the path
 * is shorter than 4 vertices.
 */
typedef boost::array<vertex*, 4> move_path;

inline std::size_t move_depth(move_path const& path) {
  return 4 - std::count(path.begin(), path.end(), static_cast<vertex*>(0));
}

inline move_path::iterator terminal(move_path& path) {
  using namespace boost::lambda;
  return std::find_if(
    path.begin(), path.end(),
    _1 == static_cast<vertex*>(0)
  ) - 1;
}

//! Try to apply the given move path to the given board. If successful, returns
//! true. Otherwise returns false and the board is left unmodified.
bool try_apply(move_path const& path, board& board);

//! Unapply a move from a board.
void unapply(move_path const& path, board& board);

//! Get the parent of the top vertex. If the top is the root, returns 0.
vertex* parent(search_stack const& stack);

//! Get the ply at which the top of the stack is.
std::size_t ply(search_stack const& stack);

//! Sort the children of the current vertex according to their heuristic
//! values.
void order(history_table const& history, vertex& v);

//! Expand the currently-selected leaf.
//!
//! \param leaf The leaf
//! \param state State in the leaf.
//! \param attacker Attacking player in the whole game.
//! \returns Number of new children.
//! \throws std::logic_error if the vertex is not a leaf.
std::size_t expand(vertex& leaf, board const& state, piece::color_t attacker);

//! Evaluate the top of the stack.
void evaluate(search_stack& stack, piece::color_t attacker, log_sink& log);

//! Attempt to find values for the top vertex in the proof table.
//! \returns true if values were found; false otherwise.
bool pt_lookup(proof_table& pt, search_stack& stack);

//! Store vertex's values in the proof table.
void pt_store(proof_table& pt, vertex const& v,
              zobrist_hasher::hash_t hash,
              std::size_t ply,
              search_stack::history_sequence::const_iterator history_begin,
              search_stack::history_sequence::const_iterator history_end);

//! Attempt to find values for given hash in the transposition table.
//! \returns True if values were found; false otherwise.
bool tt_lookup(transposition_table& tt, zobrist_hasher::hash_t hash,
               vertex& child);

//! Store a vertex's values in the transposition table.
void tt_store(transposition_table& tt, vertex const& v, 
              zobrist_hasher::hash_t hash, std::size_t ply);

//! Cut vertices in worst-first manner until enough vertices have been cut.
//! This also accepts a search_stack so that it knows which vertices to avoid
//! collecting.
//!
//! \param how_many When to stop collecting.
//! \param current_path Currently selected path -- vertices from it won't be
//!        collected.
//! \returns Number of vertices removed from the tree.
std::size_t garbage_collect(std::size_t how_many, search_stack current_path);

//! Cut all negative dis/proved children of a vertex -- that is, children
//! that are dis/proved but don't constitute a dis/proof of their parent.
//!
//! \returns Number of vertices removed from the tree.
std::size_t collect_proved(vertex& parent);

//! Cut children of the given vertex.
//! \returns Number of vertices removed from the tree.
std::size_t cut(vertex& parent);

//! Push the best successor among the top vertex's successors.
inline void push_best(search_stack& stack) {
  assert(!stack.path_top()->leaf());
  stack.push(best_successor(*stack.path_top()));
}

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
  void use_trans_tbl(boost::shared_ptr<transposition_table> const& tt) {
    trans_tbl_ = tt;
  }

  //! Get the transposition table, if any, used by this algorithm.
  //! \returns Pointer to the transposition table or null if no table is 
  //!   associated with this algorithm.
  boost::shared_ptr<transposition_table> get_trans_tbl() const {
    return trans_tbl_;
  }

  //! Make the algorithm use a proof table.
  void use_proof_tbl(boost::shared_ptr<proof_table> const& pt) {
    proof_tbl_ = pt;
  }

  //! Get the proof table, if any, used by this algorithm.
  boost::shared_ptr<proof_table> get_proof_tbl() const {
    return proof_tbl_;
  }
  
  //! Change the number of moves stored in the move cache.
#if 0
  void move_cache_size(std::size_t new_size) {
    tree_.cache_moves(new_size);
  }

  //! Get the number of moves stored in the move cache.
  std::size_t move_cache_size() const { return tree_.cached_moves(); }

  //! Get the number of hits into the move cache.
  std::size_t move_cache_hits() const { return tree_.move_cache().hits(); }

  //! Get the number of misses in the move cache.
  std::size_t move_cache_misses() const { return tree_.move_cache().misses(); }
#endif

  //! Get the total number of vertices currently held by this algorithm.
  std::size_t get_position_count() const {
    return size_;
  }

  //! Number of steps remembered by the history table.
  std::size_t history_tbl_size() const {
    return history_tbl_.size();
  }

  std::size_t gc_low() const  { return gc_low_; }
  std::size_t gc_high() const { return gc_high_; }

  void gc_low(std::size_t new_low) {
    gc_low_ = new_low;
  }

  void gc_high(std::size_t new_high) {
    gc_high_ = new_high;
  }

  //! Make this algorithm log into a sink.
  void log_into(boost::shared_ptr<log_sink> const& new_sink) {
    log_ = new_sink;
  }

  //! Get the sink this algorithm logs to.
  boost::shared_ptr<log_sink> log() const { return log_; }

  //! Run the algorithm for ms_how_long milliseconds.
  void run(unsigned ms_how_long) {
    boost::timer timer;

    while (!finished() && (ms_how_long == 0 || 
                           timer.elapsed() < ms_how_long / 1000.0))
      iterate();
  }

  void iterate() {
    if (!finished()) {
      static_cast<Algo*>(this)->do_iterate();
    }
  }

protected:
  boost::shared_ptr<apns::game>           game_;
  //! Hasher to be used during the algorithm's execution.
  zobrist_hasher                          hasher_; 
  //! Hash corresponding to initial_state.
  zobrist_hasher::hash_t                  initial_hash_;   
  search_stack                            stack_;
  boost::shared_ptr<transposition_table>  trans_tbl_;
  boost::shared_ptr<proof_table>          proof_tbl_;
  history_table                           history_tbl_;
  std::size_t                             size_;
  std::size_t                             gc_low_;
  std::size_t                             gc_high_;
  boost::shared_ptr<log_sink>             log_;

  search_algo(boost::shared_ptr<apns::game> const& game,
              std::size_t position_count = 1) 
    : game_(game)
    , initial_hash_(
        hasher_.generate_initial(game->initial_state, game->attacker)
      )
    , stack_(hasher_, initial_hash_, &game_->root, game_->attacker,
             game_->initial_state)
    , size_(position_count)
    , gc_low_(0)
    , gc_high_(0)
    , log_(new null_sink)
  { }

  void log_proof(search_stack::path_sequence::const_iterator path_begin,
                 search_stack::path_sequence::const_iterator path_end) {
    if (!log_->null()) {
      std::ostringstream out;
      format_path(out, path_begin, path_end);

      out << " proved by children\n";
      *log_ << out.str();
    }
  }

  void store_in_ht(vertex const& v) {
    vertex_counter counter;
    traverse(v, backtrack(), boost::ref(counter));
    history_tbl_.insert(*v.step, counter.count);

    *log_ << v.step->to_string()
          << " inserted into the history table with value "
          << counter.count << '\n';
  }

  void store_in_pt(
    search_stack::history_sequence::const_iterator history_begin,
    search_stack::history_sequence::const_iterator history_end,
    zobrist_hasher::hash_t hash, std::size_t ply,
    vertex const& v
  ) {
    if (proof_tbl_)
      pt_store(*proof_tbl_, v, hash, ply, history_begin, history_end);
  }

  void store_in_tt(zobrist_hasher::hash_t hash, std::size_t ply,
                   vertex const& v) {
    if (trans_tbl_)
      tt_store(*trans_tbl_, v, hash, ply);
  }

  void update_and_store(
    vertex& current, vertex* parent,
    search_stack::path_sequence::const_iterator path_begin,
    search_stack::path_sequence::const_iterator path_end,
    search_stack::history_sequence::const_iterator history_begin,
    search_stack::history_sequence::const_iterator history_end,
    zobrist_hasher::hash_t hash, std::size_t ply
   ) {
    vertex::number_t const old_pn = current.proof_number;
    vertex::number_t const old_dn = current.disproof_number;

    update_numbers(current);

    bool const numbers_changed = current.proof_number != old_pn ||
                                 current.disproof_number != old_dn;
    bool const proved = current.proof_number == 0 ||
                        current.disproof_number == 0;
    bool const cutoff = parent &&
        ((parent->type == vertex::type_or && current.proof_number == 0) ||
         (parent->type == vertex::type_and && current.disproof_number == 0));

    if (proved && numbers_changed)
      log_proof(path_begin, path_end);

    if (cutoff)
      store_in_ht(current);

    if (proved)
      store_in_pt(history_begin, history_end, ply, hash, current);
    else
      store_in_tt(hash, ply, current);
  }

  void evaluate_children() {
    vertex& current = *stack_.path_top();
    for (vertex::children_iterator child = current.children_begin();
         child != current.children_end(); ++child) {
      search_stack_checkpoint checkpoint(stack_);
      stack_.push(&*child);

      bool const found_in_pt = proof_tbl_ && pt_lookup(*proof_tbl_, stack_);
      bool const found_in_tt = trans_tbl_ && tt_lookup(*trans_tbl_,
                                                       stack_.hashes_top(),
                                                       *child);

      if (!found_in_pt && !found_in_tt)
        evaluate(stack_, game_->attacker, *log_);
    }
  }
};

//! The basic variant of the Proof-Number Search algorithm.
class proof_number_search : public search_algo<proof_number_search> {
public:
  //! Make an algorithm instance for the given game instance. 
  //!
  //! \param position_count Number of positions in the passed-in game.
  explicit proof_number_search(boost::shared_ptr<apns::game> const& game,
                               std::size_t position_count = 1) :
    search_algo(game, position_count)
  { }

  //! Perform an iteration of the algorithm.
  void do_iterate();
};

//! Depth-first variant of the Proof-Number Search algorithm.
struct depth_first_pns : public search_algo<depth_first_pns> {
  explicit depth_first_pns(boost::shared_ptr<apns::game> const& game,
                           std::size_t position_count = 1) :
    search_algo(game, position_count),
    limits_(1, limits_t(vertex::infty, vertex::infty))
  { }

  void do_iterate();
  
private:
  struct limits_t {
    vertex::number_t pn_limit;
    vertex::number_t dn_limit;

    limits_t() { }
    limits_t(vertex::number_t pn, vertex::number_t dn) 
      : pn_limit(pn)
      , dn_limit(dn) 
    { }
  };

  typedef std::vector<limits_t> limits_cont;

  limits_cont limits_;

  limits_t make_limits(vertex const& v, vertex const& parent,
                       boost::optional<vertex const&> second_best,
                       limits_t parent_limits);
};

//! A virtual algorithm -- to allow defining algos in Python.
struct virtual_algo : public search_algo<virtual_algo> {
  explicit virtual_algo(boost::shared_ptr<apns::game> const& game,
                        std::size_t position_count = 1) 
    : search_algo(game, position_count)
  { }

  virtual ~virtual_algo() { }

  void do_iterate() {
    really_do_iterate();
  }

  //! This is the function to be overriden.
  virtual void really_do_iterate() = 0;
};

} // namespace apns

#endif

