#ifndef SEARCH_ALGOS_HPP
#define SEARCH_ALGOS_HPP

#include "tree.hpp"
#include "hash.hpp"
#include "movement.hpp"
#include "config.hpp"

#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/utility.hpp>
#include <boost/timer.hpp>
#include <boost/ref.hpp>
#include <boost/circular_buffer.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>
#include <boost/function.hpp>
#include <boost/mpl/if.hpp>
#include <boost/type_traits.hpp>

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

inline int zero(vertex const&) { return 0; }

/// Return the best successor of a vertex. Can be used as a traversal policy. If there are multiple "as good" children,
/// this guarantees to select the one with highest value().
template <typename CVVertex, typename ValueF>
CVVertex* best_successor(CVVertex& parent, ValueF value) {
  typedef typename
    boost::mpl::if_<
      boost::is_const<CVVertex>,
      vertex::const_iterator,
      vertex::iterator
    >::type iterator;

  if (parent.size() > 0) {
    iterator best = parent.begin();
    int best_value = value(*best);
    vertex_comparator better(parent);

    for (iterator child = boost::next(best); child != parent.end(); ++child) {
      if (better(*child, *best) || (!better(*best, *child) && value(*child) > best_value)) {
        best = child;
        best_value = value(*child);
      }
    }

    return &*best;
  } else {
    return 0;
  }
}

template <typename CVVertex>
CVVertex* best_successor(CVVertex& parent) { return best_successor(parent, &zero); }

//! Return the best successor of a vertex and, if any, the second-best successor.
template <typename CVVertex, typename ValueF>
std::pair<CVVertex*, CVVertex*>
two_best_successors(CVVertex& parent, ValueF value) {
  typedef typename
    boost::mpl::if_<
      boost::is_const<CVVertex>,
      vertex::const_iterator,
      vertex::iterator
    >::type iterator;

  if (parent.size() >= 2) {
    vertex_comparator better(parent);

    iterator best = parent.begin();
    iterator second_best = boost::next(parent.begin());
    int best_value = value(*best);
    int second_best_value = value(*second_best);

    if (better(*second_best, *best) || second_best_value > best_value) {
      std::swap(best, second_best);
      std::swap(best_value, second_best_value);
    }

    for (vertex::iterator child = boost::next(second_best); child != parent.end(); ++child) {
      if (better(*child, *best) || (!better(*best, *child) && value(*child) > best_value)) {
        second_best = best;
        best = child;
        second_best_value = best_value;
        best_value = value(*child);
      } else if (child != best &&
                 (better(*child, *second_best) ||
                   (!better(*second_best, *child) && value(*child) > second_best_value))) {
        second_best = child;
        second_best_value = value(*child);
      }
    }

    assert(best != second_best);
    assert(&*best == best_successor(parent, value));
    return std::make_pair(&*best, &*second_best);
  } else {
    return std::make_pair(best_successor(parent, value), static_cast<vertex*>(0));
  }
}

template <typename CVVertex>
std::pair<CVVertex*, CVVertex*>
two_best_successors(CVVertex& parent) {
  return two_best_successors(parent, &zero);
}

//! Killer steps database.
class killer_db {
  struct record {
    step_holder step;
    //std::size_t hits;
  };
  typedef std::vector<record> killers_cont;

public:
  struct level_iterator : boost::iterator_facade<
    level_iterator,
    step,
    boost::random_access_traversal_tag,
    step
  > {
    level_iterator() : db_(0), level_(0), offset_(0) { }

  private:
    friend class boost::iterator_core_access;
    friend class killer_db;

    killer_db const*    db_;
    std::size_t         level_;
    std::size_t         offset_;

    level_iterator(killer_db const& db, std::size_t level, std::size_t offset)
      : db_(&db), level_(level), offset_(offset) { }

    step dereference() const { return *db_->get(level_, offset_).step; }
    bool equal(level_iterator const& other) const {
      return other.db_ == db_ && other.level_ == level_ && other.offset_ == offset_;
    }
    void            increment() { ++offset_; }
    void            decrement() { --offset_; }
    void            advance(difference_type n) { offset_ += n; }
    difference_type distance_to(level_iterator const& other) const { return other.offset_ - offset_; }
  };

  //! Make a killer DB that holds at most killer_count killers for each ply.
  explicit killer_db(std::size_t killer_count) 
    : killer_count_(0)
    , size_(0)
    , levels_(16) {
    resize_plys(killer_count);
  }

  //! Change the maximal number of killers per ply. This will drop all killers 
  //! stored in this db.
  void resize_plys(std::size_t new_killer_count) {
    if (new_killer_count != killer_count_) {
      killers_cont().swap(killers_);
      killer_count_ = new_killer_count;
      killers_.resize(killer_count_ * levels_);
      size_ = 0;
    }
  }

  //! Get the maximal number of killers in each ply.
  std::size_t levels_size() const { return killer_count_; }

  //! Number of records stored here in total for all levels and types.
  std::size_t total_size() const { return size_; }

  //! Add a step to the list of killers for given level and given parent type.
  void add(std::size_t level, step const& step);

  //! Is the given step a killer for the given level?
  bool is_killer(std::size_t level, step const& step) const;

  level_iterator level_begin(std::size_t level) const {
    if (level < levels_)
      return level_iterator(*this, level, 0);
    else
      return level_iterator();
  }

  level_iterator level_end(std::size_t level) const {
    if (level < levels_) {
      std::size_t offset = 0;
      while (offset < killer_count_ && get(level, offset).step) ++offset;
      return level_iterator(*this, level, offset);
    } else
      return level_iterator();
  };

private:
  killers_cont killers_;
  std::size_t  killer_count_;       //!< How many killers at most per ply.
  std::size_t  size_;
  std::size_t  levels_;

  record& get(std::size_t level, std::size_t offset);
  record const& get(std::size_t level, std::size_t offset) const;
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
    explicit compare(table_t const& t)
      : table_(&t) 
    { }

    bool operator () (vertex const& lhs, vertex const& rhs);

  private:
    table_t const* table_;
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
  namespace bl = boost::lambda;
  return std::find_if(path.begin(), path.end(),
                      bl::_1 == static_cast<vertex*>(0)) - 1;
}

//! Try to apply the given move path to the given board. If successful, returns
//! true. Otherwise returns false and the board is left unmodified.
bool try_apply(move_path const& path, board& board);

//! Unapply a move from a board.
void unapply(move_path const& path, board& board);

//! A sequence of hashes from the start of a move to one of its inner vertices.
//! Some elements may be set to 0 value indicating that they are to be ignored.
typedef boost::array<zobrist_hasher::hash_t, MAX_STEPS> move_history_seq;

//! Get a move history for the move that leads to the current top vertex.
move_history_seq move_history(search_stack const& stack);

//! Get the last non-zero element of a move history.
inline zobrist_hasher::hash_t last(move_history_seq const& mh) {
  namespace bl = boost::lambda;
  move_history_seq::const_reverse_iterator it =
    std::find_if(mh.rbegin(), mh.rend(), bl::_1 != 0);
  assert(it != mh.rend());
  return *it;
}

//! Get the parent of the top vertex. If the top is the root, returns 0.
vertex* parent(search_stack const& stack);

//! Get the ply at which the top of the stack is.
std::size_t ply(search_stack const& stack);

//! Sort the children of the current vertex according to their history
//! heuristic values.
void history_order(history_table const& history, vertex& v);

//! Move killers to the front of the successors list.
void killer_order(killer_db const& killers, std::size_t level, vertex& v);

//! Expand the currently-selected leaf.
//!
//! \param leaf The leaf
//! \param state State in the leaf.
//! \param attacker Attacking player in the whole game.
//! \param move_hist History of this move to avoid creating duplicate vertices.
//! \param hasher Hasher instance used to create hashes stored in move_hist.
//! \returns Number of new children.
//! \throws std::logic_error if the vertex is not a leaf.
std::size_t expand(vertex& leaf, board const& state, piece::color_t attacker,
                   move_history_seq const& move_hist,
                   zobrist_hasher const& hasher);

//! Evaluate the top of the stack.
void evaluate(search_stack& stack, piece::color_t attacker, log_sink& log);

//! Attempt to find values for the top vertex in the proof table.
//! \returns true if values were found; false otherwise.
bool pt_lookup(proof_table& pt, search_stack& stack, piece::color_t attacker);

//! Store vertex's values in the proof table.
void pt_store(proof_table& pt, vertex const& v,
              zobrist_hasher::hash_t hash, piece::color_t attacker,
              search_stack::history_sequence::const_iterator history_begin,
              search_stack::history_sequence::const_iterator history_end);

//! Attempt to find values for given hash in the transposition table.
//! \returns True if values were found; false otherwise.
bool tt_lookup(transposition_table& tt, zobrist_hasher::hash_t hash, vertex& child);

//! Store a vertex's values in the transposition table.
void tt_store(transposition_table& tt, vertex const& v, zobrist_hasher::hash_t hash);

//! Cut vertices in worst-first manner until enough vertices have been cut.
//! This also accepts a search_stack so that it knows which vertices to avoid
//! collecting.
//!
//! \param how_many When to stop collecting.
//! \param current_path Currently selected path -- vertices from it won't be collected.
void garbage_collect(std::size_t how_many, search_stack const& current_path);

//! Cut all negative dis/proved children of a vertex -- that is, children
//! that are dis/proved but don't constitute a dis/proof of their parent.
//!
//! \returns Number of vertices removed from the tree.
std::size_t collect_proved(vertex& parent, vertex const* avoid);

//! Cut children of the given vertex.
//! \returns Number of vertices removed from the tree.
std::size_t cut(vertex& parent);

//! Attempt to simulate the current top vertex.
//! \param stack The search stack whose top vertex is to be simulated.
//! \param attacker Game-wise attacker.
//! \param size Reference to the size of the whole tree; it will be updated
//!             accordingly.
//! \param killers The killer DB itself.
//! \param proof_tbl If non-null, it will be used in attempt to find a proof.
//! \param log Program-wise algorithm log sink.
//! \returns True if the top vertex has been proved; false otherwise.
bool simulate(search_stack& stack, piece::color_t attacker,
              std::size_t& size,
              killer_db const& killers,
              boost::shared_ptr<proof_table> const& proof_tbl,
              log_sink& log);

//! Push the best successor among the top vertex's successors.
template <typename ValueF>
void push_best(search_stack& stack, ValueF value) {
  assert(!stack.path_top()->leaf());
  stack.push(best_successor(*stack.path_top(), value));
}

inline void push_best(search_stack& stack) { push_best(stack, &zero); }

//! Does the given child cause a cutoff for given parent?
inline bool cutoff(vertex const& parent, vertex const& child) {
  return
    (parent.type == vertex::type_or && child.proof_number == 0) ||
    (parent.type == vertex::type_and && child.disproof_number == 0);
}

//! Is given vertex a lambda step?
inline bool is_lambda(vertex const& v) {
  return v.step == step_holder::none;
}

//! Get the depth (in levels) of the top vertex.
inline std::size_t level(search_stack const& stack) {
  return stack.path().size();
}

//! Check whether the game would be lost due to a repetition if the
//! given player made the given step from the given position assuming the
//! passed-in game history.
bool repetition(search_stack const& stack);

/// Who plays from vertex v?
piece::color_t vertex_player(vertex const& v, piece::color_t attacker);

//! A CRTP base class for search algorithms.
template <typename Algo>
class search_algo : private boost::noncopyable {
public:
  boost::shared_ptr<game> get_game() const {
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
  
  //! Make the algorithm use a killers DB.
  void use_killer_db(boost::shared_ptr<killer_db> const& killers) {
    killer_db_ = killers;
  }

  //! Get the killers db, if any, used by this algorithm.
  boost::shared_ptr<killer_db> get_killer_db() const {
    return killer_db_;
  }

  //! Get the total number of vertices currently held by this algorithm.
  std::size_t get_position_count() const {
    return game_->root.subtree_size;
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

      std::size_t const size = get_position_count();
      if (gc_enabled() && size > gc_high_ && gc_low_ < size) {
        garbage_collect(size - gc_low_, stack_);
      }
    }
  }

protected:
  struct killer_vertex_value {
    killer_vertex_value(std::size_t level, killer_db const& killers) : level_(level), killers_(&killers) { }
    int operator () (vertex const& v) const {
      if (v.step && killers_->is_killer(level_, *v.step))
        return std::numeric_limits<int>::max();
      else
        return 0;
    }

  private:
    std::size_t      level_;
    killer_db const* killers_;
  };

  boost::shared_ptr<apns::game>           game_;
  zobrist_hasher                          hasher_;        ///< Hasher to be used during the algorithm's execution.
  zobrist_hasher::hash_t                  initial_hash_;  ///< Hash corresponding to initial_state.
  search_stack                            stack_;
  boost::shared_ptr<transposition_table>  trans_tbl_;
  boost::shared_ptr<proof_table>          proof_tbl_;
  boost::shared_ptr<killer_db>            killer_db_;
  std::size_t                             gc_low_;        ///< Low GC threshold.
  std::size_t                             gc_high_;       ///< High GC threshold.
  boost::shared_ptr<log_sink>             log_;           ///< Logger to be used.

  search_algo(boost::shared_ptr<apns::game> const& game)
    : game_(game)
    , initial_hash_(
        hasher_.generate_initial(game->initial_state, game->attacker, MAX_STEPS)
      )
    , stack_(hasher_, initial_hash_, &game_->root, game_->attacker, game_->initial_state)
    , gc_low_(0)
    , gc_high_(0)
    , log_(new null_sink) {
#ifndef NDEBUG
    vertex_counter counter;
    traverse(game_->root, backtrack(), boost::ref(counter));
    assert(counter.count == game_->root.subtree_size);
#endif
  }

  bool gc_enabled() const { return gc_high_ > 0; }

  void log_proof(search_stack::path_sequence::const_iterator path_begin,
                 search_stack::path_sequence::const_iterator path_end) {
    if (!log_->null()) {
      std::ostringstream out;
      format_path(out, path_begin, path_end);

      out << " proved by children\n";
      *log_ << out.str();
    }
  }

  void store_in_killer_db(std::size_t level, vertex const& v) {
    if (killer_db_ && v.step) {
      killer_db_->add(level, *v.step);

      *log_ << v.step->to_string()
            << " at level " << level << '\n';
    }
  }

  void store_in_pt(
    search_stack::history_sequence::const_iterator history_begin,
    search_stack::history_sequence::const_iterator history_end,
    zobrist_hasher::hash_t hash, vertex const& v
  ) {
    if (proof_tbl_ && v.step)
      pt_store(*proof_tbl_, v, hash, game_->attacker, history_begin, history_end);
  }

  void store_in_tt(zobrist_hasher::hash_t hash, vertex const& v) {
    if (trans_tbl_ && v.step)
      tt_store(*trans_tbl_, v, hash);
  }

  void update_and_store(
    vertex& current, vertex* parent,
    search_stack::path_sequence::const_iterator path_begin,
    search_stack::path_sequence::const_iterator path_end,
    search_stack::history_sequence::const_iterator history_begin,
    search_stack::history_sequence::const_iterator history_end,
    zobrist_hasher::hash_t hash, std::size_t size_increment
  ) {
    vertex::number_t const old_pn = current.proof_number;
    vertex::number_t const old_dn = current.disproof_number;

    update_numbers(current);
    current.subtree_size += size_increment;

    if (!is_lambda(current)) {
      bool const numbers_changed = current.proof_number != old_pn || current.disproof_number != old_dn;
      bool const proved = current.proof_number == 0 || current.disproof_number == 0;
      bool const cutoff = parent && apns::cutoff(*parent, current);

      if (proved && numbers_changed)
        log_proof(path_begin, path_end);

      std::size_t const level = std::distance(path_begin, path_end);
      if (cutoff && parent)
        store_in_killer_db(level - 1, current);

      if (proved)
        store_in_pt(history_begin, history_end, hash, current);
      else
        store_in_tt(hash, current);
    }
  }

  std::size_t expand_and_eval() {
    std::size_t size = 0;

#if KILLER_SIMULATE
    bool const simulated = killer_db_ && simulate(stack_, game_->attacker, size, *killer_db_, proof_tbl_, *log_);
#else
    bool const simulated = false;
#endif

    if (!simulated) {
      assert(stack_.path_top()->leaf());

      size = expand(
        *stack_.path_top(), stack_.state(), game_->attacker,
        move_history(stack_), hasher_
      );

#if KILLER_SORT_AFTER_EXPAND
      if (killer_db_)
        killer_order(*killer_db_, stack_.size(), *stack_.path_top());
#endif

      evaluate_children();
    }

    return size;
  }

  void evaluate_children() {
    vertex& current = *stack_.path_top();
    for (vertex::iterator child = current.begin(); child != current.end(); ++child) {
      piece::color_t const attacker = game_->attacker;
      piece::color_t const player = vertex_player(current, attacker);

      if (child->type != current.type && repetition(stack_)) {
        *log_ << stack_ << " proved by repetition\n";

        child->proof_number = player == attacker ? vertex::infty : 0;
        child->disproof_number = player == attacker ? 0 : vertex::infty;
      }
      else if (!is_lambda(*child)) {
        search_stack_checkpoint checkpoint(stack_);
        stack_.push(&*child);

        bool const found_in_pt = proof_tbl_ && pt_lookup(*proof_tbl_, stack_, game_->attacker);
        if (!found_in_pt) {
          bool const found_in_tt = trans_tbl_ && tt_lookup(*trans_tbl_, stack_.hashes_top(), *child);

          if (!found_in_tt)
            evaluate(stack_, game_->attacker, *log_);
        } else {
          // found_in_pt
          *log_ << stack_ << " proved by proof table\n";
        }
      }

      if (cutoff(current, *child))
        store_in_killer_db(stack_.size() - 1, *child);
    }
  }

  void select_best() {
#if KILLER_SORT_BEFORE_SELECT
    if (killer_db)
      killer_order(*killer_db_, stack_.size(), *stack_.path_top());
#endif

#if KILLER_PREFER
    if (killer_db_)
      push_best(stack_, killer_vertex_value(stack_.size(), *killer_db_));
    else
      push_best(stack_);
#else
    push_best(stack_);
#endif
  }
};

//! The basic variant of the Proof-Number Search algorithm.
class proof_number_search : public search_algo<proof_number_search> {
public:
  //! Make an algorithm instance for the given game instance. 
  //!
  //! \param position_count Number of positions in the passed-in game.
  explicit proof_number_search(boost::shared_ptr<apns::game> const& game)
    : search_algo(game) { }

  //! Perform an iteration of the algorithm.
  void do_iterate();
};

//! Depth-first variant of the Proof-Number Search algorithm.
struct depth_first_pns : public search_algo<depth_first_pns> {
  explicit depth_first_pns(boost::shared_ptr<apns::game> const& game)
    : search_algo(game)
    , limits_(1, limits_t(vertex::infty, vertex::infty)) { }

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
  explicit virtual_algo(boost::shared_ptr<apns::game> const& game)
    : search_algo(game) { }

  virtual ~virtual_algo() { }

  void do_iterate() {
    really_do_iterate();
  }

  //! This is the function to be overriden.
  virtual void really_do_iterate() = 0;
};

} // namespace apns

#endif

