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
vertex const* best_successor(vertex const& parent);

//! Return the best successor of a vertex and, if any, the second-best successor.
std::pair<vertex const*, vertex const*> two_best_successors(vertex const& parent);

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

  //! How long is the current path?
  std::size_t size() const { return path_.size(); }

  path_sequence const&    path() const    { return path_; }
  hashes_sequence const&  hashes() const  { return hashes_; }
  history_sequence const& history() const { return history_; }
  board const&            state() const   { return state_; }

  vertex*                 path_top() const    { return path_.back(); }
  zobrist_hasher::hash_t  hashes_top() const  { return hashes_.back(); }
  zobrist_hasher::hash_t  history_top() const { return history_.back(); }

  zobrist_hasher const& hasher() const { return *hasher_; }

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

//! A search tree is more than just a wrapper around a pointer to the tree's root: It cooperates with transposition and proof
//! tables as well as killer table. It takes care of the tree operations in a manner that is aware of what's inside the tree
//! and what heuristics are enabled. It assumes that during its lifetime it is the only thing that modifies the tree. It,
//! however, does not take ownership of the tree.
class search_tree : boost::noncopyable {
public:
  search_tree(vertex* root, piece::color_t attacker, std::size_t size, zobrist_hasher const& hasher,
              zobrist_hasher::hash_t initial_hash, board const& initial_state) :
    attacker_(attacker),
    size_(size),
    stack_(hasher, initial_hash, root, attacker, initial_state)
  { }

  vertex const& current() const { return *stack_.path_top(); }

  //! Make a child of the current vertex the new current vertex. The behaviour is undefined if #child is not the current
  //! vertex's child.
  void select_child(vertex::const_children_iterator child) { select_child(&*child); }
  void select_child(vertex const* child) { stack_.push(const_cast<vertex*>(child)); }

  //! Make the parent of the current vertex the new current vertex.
  //! \throws std::logic_error if root is currently selected
  void select_parent() { stack_.pop(); }

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

  //! Cut the current vertex's children. Children that constitute proof or disproof are not cut.
  void cut_children();

  //! Update vertices' numbers along the current path.
  void update_path();

  void use_trans_tbl(std::size_t size)        { trans_tbl_.reset(new transposition_table(size)); }
  void use_proof_tbl(std::size_t size)        { proof_tbl_.reset(new proof_table(size)); }
  void use_killers(std::size_t size_per_ply)  { killers_.reset(new killer_db(size_per_ply)); }

  transposition_table const*  trans_tbl() const   { return trans_tbl_.get(); }
  proof_table const*          proof_tbl() const   { return proof_tbl_.get(); }
  killer_db const*            killers() const     { return killers_.get(); }

  std::size_t size() const { return size_; }

private:
  piece::color_t  attacker_;
  std::size_t     size_;
  search_stack    stack_;

  boost::shared_ptr<transposition_table>  trans_tbl_;
  boost::shared_ptr<proof_table>          proof_tbl_;
  boost::shared_ptr<killer_db>            killers_;

  bool simulate();
  void make_child(vertex& child, step const& step, vertex::e_type type);
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
  void use_trans_tbl(std::size_t size) {
    tree_.use_trans_tbl(size);
  }

  //! Get the transposition table, if any, used by this algorithm.
  //! \returns Pointer to the transposition table or null if no table is associated with this algorithm.
  transposition_table const* get_trans_tbl() const {
    return tree_.trans_tbl();
  }

  //! Make the algorithm use a proof table.
  void use_proof_tbl(std::size_t size) {
    tree_.use_proof_tbl(size);
  }

  //! Get the proof table, if any, used by this algorithm.
  proof_table const* get_proof_tbl() const {
    return tree_.proof_tbl();
  }
  
  //! Change the number of killers stored for each ply.
  void set_killer_count(std::size_t new_killer_count) {
    tree_.use_killers(new_killer_count);
  }

  //! Get the max. number of killers stored for each ply.
  std::size_t get_killer_count() const {
    return tree_.killers()->plys_size();
  }

  //! Get the total number of vertices currently held by this algorithm.
  std::size_t get_position_count() const {
    return tree_.size();
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
  boost::shared_ptr<apns::game> game_;
  zobrist_hasher                hasher_;         //!< Hasher to be used during the algorithm.
  zobrist_hasher::hash_t        initial_hash_;   //!< Hash corresponding to initial_state.
  search_tree                   tree_;

  search_algo(boost::shared_ptr<apns::game> const& game, std::size_t position_count = 1) :
    game_(game),
    initial_hash_(hasher_.generate_initial(game->initial_state, game->attacker)),
    tree_(&game->root, game->attacker, position_count, hasher_, initial_hash_, game->initial_state)
  { }
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
    limits_(1, limits_t(vertex::infty, vertex::infty))
  { }

  void do_iterate();
  
private:
  struct limits_t {
    vertex::number_t pn_limit;
    vertex::number_t dn_limit;

    limits_t() { }
    limits_t(vertex::number_t pn, vertex::number_t dn) : pn_limit(pn), dn_limit(dn) { }
  };

  typedef std::vector<limits_t> limits_cont;

  limits_cont limits_;

  limits_t make_limits(vertex const& v, vertex const& parent,
                       boost::optional<vertex const&> second_best, limits_t parent_limits);
};

} // namespace apns

#endif

