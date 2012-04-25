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

namespace apns {

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
  explicit vertex_comparator(vertex const& parent) :
    number_(parent.type == vertex::type_or ? 
            &vertex::proof_number : &vertex::disproof_number),
    parent_type_(parent.type)
  { }

  bool operator () (vertex const& lhs, vertex const& rhs) {
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
vertex* best_successor(vertex& parent);
vertex const* best_successor(vertex const& parent);

//! Return the best successor of a vertex and, if any, the second-best
//! successor.
std::pair<vertex const*, vertex const*>
two_best_successors(vertex const& parent);

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

    assert(!"Wont' get here");
    return killers_or_;
  }
};

//! History table serves for step-ordering within a vertex.
class history_table {
public:
  //! Advice the table that step caused a cut-off at depth.
  void insert(step const& step, std::size_t depth);

  //! Sort the children of a vertex.
  void sort(vertex& v);

private:
  typedef boost::unordered_map<step, boost::uint64_t> table_t;
  struct compare {
    table_t* table;
    explicit compare(table_t& t) : table(&t) { }
    bool operator () (vertex const& lhs, vertex const& rhs);
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

  //! Push a successor of the currently-last vertex.
  void push(vertex* v); 

  //! Pop a vertex. The stack must not be at root before this call.
  void pop();           

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

//! All possible moves from one position for one player. This serves as a kind
//! of a cache.
class player_moves {
public:
  explicit player_moves(board const& root_position) 
    : root_position_(root_position) i
  { }

  //! Insert a move into the table.
  //! \param hash Zobrist's hash value for the terminal vertex of the path.
  void insert(zobrist_hasher::hash_t hash, move_path const& path) {
    moves_.insert(std::make_pair(hash, path)); 
  }

  //! Remove a move from the table.
  void remove(zobrist_hasher::hash_t hash) { moves_.erase(hash); }

  //! Query for a pre-stored move in the table.
  //! \param hash Key used for the query.
  //! \param expected_position The expected target position for verification in
  //!   the even of hash collisions.
  boost::optional<move_path>
  query(zobrist_hasher::hash_t hash, board const& expected_position);

  //! The move-root position for this move.
  board const& root_position() const { return root_position_; }

  //! Number of moves stored in this table.
  std::size_t size() const { return moves_.size(); }

private:
  typedef boost::unordered_map<zobrist_hasher::hash_t, move_path> moves_map;
  moves_map moves_;
  board     root_position_;

  bool verify(move_path const& found_path, board const& expected);
};

//! Cache of moves in the tree.
class moves {
public:
  explicit moves(std::size_t moves_cached) :
    moves_cached_(moves_cached),
    time_(0),
    hits_(0),
    misses_(0)
  { }

  //! Get moves for the given move.
  //! \param ply The ply.
  //! \param root_hash Hash value of the move root vertex.
  //! \param root_pos The board position in the move root vertex.
  //! \param root The move-root vertex itself.
  //! \param hasher Hasher used for making the hashes.
  //! \param attacker Attacker for the whole search tree (that is, *not* the 
  //!   player who moves from the given root).
  boost::shared_ptr<player_moves>
  get(zobrist_hasher::hash_t root_hash, board const& root_pos,
      vertex* root, zobrist_hasher const& hasher, piece::color_t attacker);

  //! Remove moves.
  void remove(zobrist_hasher::hash_t root_hash);

  //! Set how many moves to cache.
  void moves_cached(std::size_t new_moves_cached);

  //! How many moves are currently cached?
  std::size_t size() const { return records_.size(); }

  //! Get how many moves at most are currently cached.
  std::size_t moves_cached() const { return moves_cached_; }

  std::size_t hits() const    { return hits_; }
  std::size_t misses() const  { return misses_; }

private:
  struct move_record {
    unsigned                        last_used;
    boost::shared_ptr<player_moves> moves;

    move_record() : last_used(0) { }
    move_record(unsigned last_used,
                boost::shared_ptr<player_moves> const& moves) 
      : last_used(last_used)
      , moves(moves)
    { }
  };

  typedef boost::unordered_map<
    zobrist_hasher::hash_t, move_record
  > records_map;

  records_map records_;
  std::size_t moves_cached_;
  std::size_t time_;
  std::size_t hits_;
  std::size_t misses_;

  void gather(boost::shared_ptr<player_moves> const& moves,
              search_stack& stack);
  void resize_records(std::size_t new_size);
  bool records_compare(records_map::value_type const& lhs,
                       records_map::value_type const& rhs);
};

/** A search tree is more than just a wrapper around a pointer to the tree's
 * root: It cooperates with transposition and proof tables as well as killer
 * table. It takes care of the tree operations in a manner that is aware of
 * what's inside the tree and what heuristics are enabled. It assumes that
 * during its lifetime it is the only thing that modifies the tree. It,
 * however, does not take ownership of the tree.
 */
class search_tree : boost::noncopyable {
  static std::size_t const DEFAULT_MOVES_CACHED_ = 32;

public:
  search_tree(vertex* root, piece::color_t attacker, std::size_t size, 
              zobrist_hasher const& hasher,
              zobrist_hasher::hash_t initial_hash, board const& initial_state) 
    : attacker_(attacker)
    , size_(size)
    , stack_(hasher, initial_hash, root, attacker, initial_state)
    , hasher_(&hasher)
    , history_(new history_table)
    , moves_(DEFAULT_MOVES_CACHED_)
  { }

  vertex const& current() const { return *stack_.path_top(); }

  //! Make a child of the current vertex the new current vertex. The behaviour 
  //! is undefined if #child is not the current vertex's child.
  void select_child(vertex::const_children_iterator child) {
    select_child(&*child); 
  }

  void select_child(vertex const* child) {
    stack_.push(const_cast<vertex*>(child)); 
  }

  //! Make the parent of the current vertex the new current vertex.
  //! \throws std::logic_error if root is currently selected
  void select_parent() { stack_.pop(); }

  //! See the parent vertex without selecting it. The behaviour is undefined if 
  //! this tree is currently .at_root().
  vertex const& parent() const { return **(stack_.path().rbegin() + 1); }

  //! Drop the current path and select root.
  void select_root() { while (!at_root()) select_parent(); }

  //! Is root currently selected?
  bool at_root() const { return stack_.at_root(); }

  //! How deep is the currently-selected vertex?
  std::size_t selection_depth() const { return stack_.size(); }

  //! Expand the currently-selected leaf.
  //!
  //! \throws std::logic_error if the currently selected vertex is not a leaf.
  void expand();

  //! Evaluate the currently-selected vertex.
  void evaluate();

  //! Evaluate every child of the currently-selected vertex.
  void evaluate_children();

  //! Attempt to find values for the currently-selected vertex in the tables, 
  //! but not evaluate it if the look-up fails.
  //! \returns true if values were found; false otherwise.
  bool evaluate_cached();

  //! Cut the current vertex's children. Children that constitute proof or 
  //! disproof are not cut.
  void cut_children();

  //! Assuming the current vertex has just been expanded (contains only 
  //! leaves), reduce the tree -- discard vertices that ultimately represent 
  //! the same move.
  void reduce();

  //! Update vertices' numbers along the current path.
  void update_path();

  void use_trans_tbl(std::size_t size) {
    trans_tbl_.reset(new transposition_table(size)); 
  }

  void use_proof_tbl(std::size_t size) {
    proof_tbl_.reset(new proof_table(size)); 
  }

  void cache_moves(std::size_t how_many) {
    moves_.moves_cached(how_many); 
  }

  transposition_table const* trans_tbl() const {
    return trans_tbl_.get(); 
  }

  proof_table const* proof_tbl() const {
    return proof_tbl_.get(); 
  }

  std::size_t cached_moves() const {
    return moves_.moves_cached(); 
  }

  moves const& move_cache() const { return moves_; }

  std::size_t size() const { return size_; }

private:
  piece::color_t        attacker_;
  std::size_t           size_;
  search_stack          stack_;
  zobrist_hasher const* hasher_;

  boost::shared_ptr<transposition_table>  trans_tbl_;
  boost::shared_ptr<proof_table>          proof_tbl_;
  boost::shared_ptr<history_table>        history_;
  moves                                   moves_;

  //! Get the move root of the currently-selected vertex.
  //! \param root Will be set to the move-root vertex.
  //! \param root_hash Will be set to move-root's hash.
  //! \param position Will be set to the move-root's position.
  //! \param move_len Will be set to the number of steps in the move.
  void find_move_root(vertex*& root, zobrist_hasher::hash_t& root_hash, 
                      board& position, std::size_t& move_len);

  //! Store the current child to the moves table.
  void store_move(boost::shared_ptr<player_moves> const& moves, 
                  std::size_t current_depth);
};

inline void select_best(search_tree& tree) {
  assert(!tree.current().leaf());
  tree.select_child(best_successor(tree.current()));
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
  void use_trans_tbl(std::size_t size) { tree_.use_trans_tbl(size); }

  //! Get the transposition table, if any, used by this algorithm.
  //! \returns Pointer to the transposition table or null if no table is 
  //!   associated with this algorithm.
  transposition_table const* get_trans_tbl() const {
    return tree_.trans_tbl(); 
  }

  //! Make the algorithm use a proof table.
  void use_proof_tbl(std::size_t size) { tree_.use_proof_tbl(size); }

  //! Get the proof table, if any, used by this algorithm.
  proof_table const* get_proof_tbl() const { return tree_.proof_tbl(); }
  
  //! Change the number of moves stored in the move cache.
  void move_cache_size(std::size_t new_size) { tree_.cache_moves(new_size); }

  //! Get the number of moves stored in the move cache.
  std::size_t move_cache_size() const { return tree_.cached_moves(); }

  //! Get the number of hits into the move cache.
  std::size_t move_cache_hits() const { return tree_.move_cache().hits(); }

  //! Get the number of misses in the move cache.
  std::size_t move_cache_misses() const { return tree_.move_cache().misses(); }

  //! Get the total number of vertices currently held by this algorithm.
  std::size_t get_position_count() const {
    return tree_.size();
  }

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
  boost::shared_ptr<apns::game> game_;
  //! Hasher to be used during the algorithm's execution.
  zobrist_hasher                hasher_; 
  //! Hash corresponding to initial_state.
  zobrist_hasher::hash_t        initial_hash_;   
  search_tree                   tree_;

  search_algo(boost::shared_ptr<apns::game> const& game,
              std::size_t position_count = 1) 
    : game_(game)
    , initial_hash_(
        hasher_.generate_initial(game->initial_state, game->attacker)
      )
    , tree_(
        &game->root, game->attacker, position_count, hasher_, initial_hash_, 
        game->initial_state
      )
  { }
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

