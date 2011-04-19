#ifndef SEARCH_HPP
#define SEARCH_HPP

#include "board.hpp"
#include "movement.hpp"
#include "hash.hpp"

#include <boost/scoped_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/utility.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/timer.hpp>
#include <boost/python.hpp>

#include <vector>
#include <limits>
#include <stack>
#include <utility>
#include <stdexcept>
#include <memory>

#include <iostream>

//! One vertex of the search tree.
class vertex : boost::noncopyable {
  struct parent_node;

public:
  //! Type of the proof- and disproof-number values.
  typedef unsigned number_t;

  //! Maximum value that number_t can hold.
  static number_t const max_num;

  //! Infinity representation.
  static number_t const infty;

  //! Iterator type over the children list.
  typedef vertex** children_iterator;

  //! Iterator type over the parents list.
  class parent_iterator : public boost::iterator_facade<
    parent_iterator,
    vertex*,
    boost::forward_traversal_tag
  > {
  public:
    parent_iterator() : current(0) { }  //!< Construct the singular iterator.

  private:
    friend class boost::iterator_core_access;
    friend class vertex;

    explicit parent_iterator(parent_node* current);

    void increment();
    reference dereference() const;
    bool equal(parent_iterator const& other) const;

    parent_node* current;
  };

  vertex();

  children_iterator children_begin();   //!< Beginning of the sequence of children.
  children_iterator children_end();     //!< End of the sequence of children.

  parent_iterator parents_begin();      //!< Beginning of the sequence of parents.
  parent_iterator parents_end();        //!< End of the sequence of parents.

  //! Type of this vertex.
  enum e_type {
    type_and,
    type_or
  } type;

  number_t proof_number;        //!< Proof-number of this vertex.
  number_t disproof_number;     //!< Disproof-number of this vertex.
  int steps_remaining;          //!< How many steps does this player have left until the end of their move?
  bool visited;                 //!< Has this vertex been visited in the current iteration?

  boost::optional<step> leading_step;  //!< The step that has led to this state.

  //! Allocate specified amount of children and default-initialise them.
  void alloc_children(std::size_t count);

  //! Add the given vertex as a new parent of this one.
  void add_parent(vertex* parent);

private:
  //! One node of the linked list of parents.
  struct parent_node {
    vertex*                     parent;
    std::auto_ptr<parent_node>  next;

    explicit parent_node(vertex* parent = 0, std::auto_ptr<parent_node> next = std::auto_ptr<parent_node>())
      : parent(parent)
      , next(next)
    { }
  };

  //! Remove a parent from the list of parents. This function assumes the specified parent does exist in the list.
  void remove_parent(vertex* parent);

  friend void delete_subtree(vertex*);  // Give #delete_subtree access to #remove_parent.

  std::auto_ptr<parent_node>    parent_list;
  boost::scoped_array<vertex*>  children;
  std::size_t                   children_size;
};

//! Pointer to vertex type.
typedef vertex* vertex_ptr;

//! Delete all possible vertices rooted at #root. This does not necessarily deallocate the whole subtree -- if a vertex has
//! multiple parents, and only one of them is removed, the vertex is still kept in memory. This function uses operator delete
//! to deallocate vertices.
void delete_subtree(vertex* root);

//! Find the number-minimal child in the sequence [begin, end). number may be either &vertex::proof_number or disproof_number.
vertex* find_min(vertex::children_iterator begin, vertex::children_iterator end, vertex::number_t vertex::*number);

//! Given a type of one vertex (AND or OR vertex), return the opposite type.
vertex::e_type opposite_type(vertex::e_type to_what);

//! Pair of proof and disproof number.
typedef std::pair<vertex::number_t, vertex::number_t> pn_dn_pair_t;

/**
 * The Proof-Number Search algorithm.
 */
template <typename Strategy, typename Hasher>
class pn_search_algo : boost::noncopyable {
public:
  typedef transposition_table<Hasher> transposition_table_t;
  typedef typename transposition_table_t::pointer       trans_tbl_ptr;
  typedef typename transposition_table_t::const_pointer trans_tbl_const_ptr;

  pn_search_algo(board const& initial_board, piece::color_t player, Strategy strategy);
  pn_search_algo(board const& initial_board, vertex* tree, piece::color_t player, Strategy strategy, unsigned position_count);
  ~pn_search_algo();

  void run(unsigned ms_how_long);
  bool finished() const;

  void iterate();

  vertex* get_root() const                { return root; }
  board const& get_initial_board() const  { return initial_board; }
  piece::color_t get_player() const       { return player; }
  std::size_t get_position_count() const  { return position_count; }
  Strategy const& get_strategy() const    { return strategy; }

  static std::size_t get_size_of_trans_tbl_element() { return transposition_table_t::SIZE_OF_ELEMENT; }
  void use_transposition_table(std::size_t elements, std::size_t keep_time);
  transposition_table_t const* get_transposition_table() const { return trans_tbl.get(); }

private:
  typedef typename Hasher::hash_t hash_t;

  struct history_record {
    board               position;       //!< Placement of the pieces.
    piece::color_t      player;         //!< Which player's turn is it in this position?

    history_record( ::board const& position, piece::color_t player)
      : position(position)
      , player(player)
    { }
  };

  typedef std::vector<history_record> history_seq;
  typedef std::vector<vertex*> path_seq;

  Hasher                        hasher;
  Strategy                      strategy;
  vertex*                       root;
  board const&                  initial_board;
  hash_t                        initial_hash;
  piece::color_t                player;
  trans_tbl_ptr                 trans_tbl;
  std::size_t                   position_count;

  //! Find the best leaf for expansion. This function sets the visited flag on each traversed vertex to true.
  //! \param board Must be passed the initial board. This parameter will be modified in-place. Upon return, it will contain the
  //!     board corresponding to the selected leaf.
  //! \param leaf A vertex_ptr object. This will be set to the found leaf upon return.
  //! \param leaf_hash Hash of #leaf.
  //! \param history Upon return, this will contain the sequence of all first steps on the path from root to the leaf.
  //!     Any previous contents of this container will be overwritten.
  //! \param path All vertices on the path from root to leaf.
  void find_leaf(board& board, vertex_ptr& leaf, hash_t& leaf_hash, history_seq& history, path_seq& path);

  //! Expand a leaf.
  //! \param board Board corresponding to #leaf.
  //! \param leaf The leaf.
  //! \param leaf_hash Hash of #leaf.
  //! \param history All first steps on the root -> leaf path.
  void expand(board& board, vertex_ptr leaf, hash_t leaf_hash, history_seq const& history);

  //! Traverse the tree from #leaf to root, updating proof- and disproof-number values.
  void update_numbers(vertex_ptr leaf);
};

struct win_strategy {
  pn_dn_pair_t updated_numbers(vertex_ptr vertex) const;
  pn_dn_pair_t initial_numbers(board const& board,
      piece::color_t initial_player, piece::color_t from_player, piece::color_t to_player) const;
};

namespace detail {

void add_sum(vertex::number_t& sum, vertex::number_t addend);
vertex::number_t add(vertex::number_t first, vertex::number_t second);
void add_sum_no_infty(vertex::number_t& sum, vertex::number_t addend);

//! Get the board corresponding to a given vertex. That means traversing the whole tree from the given vertex up to root,
//! then back down, transforming the initial board.
board board_from_vertex(vertex_ptr v, board const& initial_board);

}

template <typename Strategy, typename Hasher>
pn_search_algo<Strategy, Hasher>::pn_search_algo(board const& initial_board, piece::color_t player, Strategy strategy)
try
  : strategy(strategy)
  , root(new vertex)
  , initial_board(initial_board)
  , initial_hash(hasher.generate_initial(initial_board, player))
  , player(player)
  , position_count(1)
{
  root->type = vertex::type_or;
  pn_dn_pair_t pn_dn = strategy.initial_numbers(initial_board, player, opponent_color(player), player);
  root->proof_number = pn_dn.first;
  root->disproof_number = pn_dn.second;
  root->steps_remaining = MAX_STEPS;
} catch (...) {
  delete root;
  throw;
}

template <typename Strategy, typename Hasher>
pn_search_algo<Strategy, Hasher>::pn_search_algo(board const& initial_board,
    vertex_ptr tree, piece::color_t player, Strategy strategy, unsigned position_count)
  : strategy(strategy)
  , root(tree)
  , initial_board(initial_board)
  , initial_hash(hasher.generate_initial(initial_board, player))
  , player(player)
  , position_count(position_count)
{ }

template <typename Strategy, typename Hasher>
pn_search_algo<Strategy, Hasher>::~pn_search_algo() {
  delete_subtree(root);
  root = 0;
}

template <typename Strategy, typename H>
void pn_search_algo<Strategy, H>::run(unsigned ms_how_long) {
  boost::timer timer;

  while (!finished() && (ms_how_long == 0 || timer.elapsed() < ms_how_long / 1000.0)) {
    iterate();
  }
}

template <typename Strategy, typename H>
bool pn_search_algo<Strategy, H>::finished() const {
  return root->proof_number == 0 || root->disproof_number == 0;
}

template <typename Strategy, typename H>
void pn_search_algo<Strategy, H>::iterate() {
  board board = initial_board;

  if (trans_tbl) {
    trans_tbl->tick();
  }

  history_seq history;
  path_seq path;

  vertex_ptr leaf;
  hash_t leaf_hash = initial_hash;
  find_leaf(board, leaf, leaf_hash, history, path);
  expand(board, leaf, leaf_hash, history);
  update_numbers(leaf);

  for (path_seq::const_iterator vertex = path.begin(); vertex != path.end(); ++vertex) {
    assert((*vertex)->visited);
    (*vertex)->visited = false;
  }
}

template <typename Strategy, typename H>
void pn_search_algo<Strategy, H>::find_leaf(board& board, vertex_ptr& leaf, hash_t& leaf_hash, history_seq& history,
    path_seq& path) {
  leaf = root;
  vertex_ptr current = root;
  board = initial_board;
  leaf_hash = initial_hash;
  piece::color_t last_player = player;
  history.clear();
  path.clear();

  history.push_back(history_record(board, player));

  while (current) {
    piece::color_t const current_player = current->type == vertex::type_or ? player : opponent_color(player);

    current->visited = true;
    path.push_back(current);

    // The player has changed. This is the first step of a move.
    if (current_player != last_player) {
      history.push_back(history_record(board, current_player));
    }

    if (current->leading_step) {
      apply(*current->leading_step, board);
      leaf_hash = hasher.update(leaf_hash,
          current->leading_step->step_sequence_begin(),
          current->leading_step->step_sequence_end(),
          last_player, current_player);
    } else {
      assert(current == root);
    }

    last_player = current_player;
    leaf = current;

    vertex::number_t vertex::* number = current->type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number;
    current = find_min(current->children_begin(), current->children_end(), number);
  }
}

template <typename Strategy, typename H>
void pn_search_algo<Strategy, H>::expand(board& board, vertex_ptr leaf, hash_t leaf_hash, history_seq const& history) {
  using detail::board_from_vertex;

  assert(leaf->children_begin() == leaf->children_end());
  assert(!(leaf->proof_number == 0 || leaf->disproof_number == 0));

  piece::color_t const player = (leaf->type == vertex::type_or ? this->player : opponent_color(this->player));
  piece::color_t const opponent = opponent_color(player);

  // Build a list of all possible steps first. Store the desired target vertex type with each step.
  typedef std::vector<std::pair<step, vertex::e_type> > steps_seq;
  steps_seq steps;

  for (all_steps_iter step = all_steps_begin(board, player); step != all_steps_end(); ++step) {
    if (leaf->steps_remaining - (signed)step->steps_used() >= 1) {
      // The player can either make this step and keep playing or make it and let the opponent play.
      steps.push_back(std::make_pair(*step, leaf->type));
      steps.push_back(std::make_pair(*step, opposite_type(leaf->type)));
    } else if (leaf->steps_remaining - (signed)step->steps_used() >= 0) {
      // This player can only make this step if it is the last step in their move.
      steps.push_back(std::make_pair(*step, opposite_type(leaf->type)));
    }
  }

  // Now attach all these steps as children of the leaf.
  leaf->alloc_children(steps.size());

  steps_seq::const_iterator s = steps.begin();
  vertex::children_iterator child = leaf->children_begin();
  for (; s != steps.end() && child != leaf->children_end(); ++s, ++child) {
    step const& step = s->first;
    vertex::e_type const type = s->second;

    apply(step, board);

    std::auto_ptr<vertex> new_vertex(new vertex);
    new_vertex->leading_step = step;
    new_vertex->type = type;

    if (new_vertex->type == leaf->type) {
      new_vertex->steps_remaining = leaf->steps_remaining - (signed)step.steps_used();
    } else {
      new_vertex->steps_remaining = MAX_STEPS - (signed)step.steps_used();
    }

    ++position_count;

    bool pn_dn_set = false;

    hash_t new_hash = hasher.update(leaf_hash,
        step.step_sequence_begin(), step.step_sequence_end(),
        player,
        type == leaf->type ? player : opponent);
    if (trans_tbl) {
      vertex* record = trans_tbl->query(new_hash);
      if (record
          && record->steps_remaining == new_vertex->steps_remaining
          && board_from_vertex(record, initial_board) == board
          && record->proof_number != 0
          && record->disproof_number != 0) {
        new_vertex->proof_number = record->proof_number;
        new_vertex->disproof_number = record->disproof_number;
        pn_dn_set = true;
        trans_tbl->hit();
      }
    }

    if (!pn_dn_set) {
      trans_tbl->miss();
    }

    // Check for repetitions.
    bool lose = false;

    if (!history.empty() && type != leaf->type) {  // Only check for repetitions if this is the end of the move.
      if (history.back().position == board && history.back().player == player) {
        lose = true;  // Lose because a move must lead to a net change in overall position.
      } else {
        // Check for third-time repetitions.
        unsigned count = 0;
        for (typename history_seq::const_iterator h = history.begin(); h != history.end(); ++h) {
          if (h->player == player && h->position == board) {
            ++count;
          }
        }

        if (count >= 3) {
          lose = true;
        }
      }
    }

    if (!lose && !pn_dn_set) {
      pn_dn_pair_t const numbers = strategy.initial_numbers(board, this->player, player,
          new_vertex->type == leaf->type ? player : opponent);
      new_vertex->proof_number = numbers.first;
      new_vertex->disproof_number = numbers.second;

      if (trans_tbl && new_vertex->proof_number != 0 && new_vertex->disproof_number != 0) {
        trans_tbl->insert(new_hash, new_vertex.get());
      }
    } else if (lose) {
      new_vertex->proof_number    = player == this->player ? vertex::infty : 0;
      new_vertex->disproof_number = player == this->player ? 0 : vertex::infty;
    }

    new_vertex->add_parent(leaf);
    unapply(step, board);
    *child = new_vertex.release();
  }
}

template <typename Strategy, typename H>
void pn_search_algo<Strategy, H>::update_numbers(vertex_ptr start) {
  std::stack<vertex_ptr> stack;
  stack.push(start);

  while (!stack.empty()) {
    vertex_ptr vertex = stack.top();
    stack.pop();

    pn_dn_pair_t const numbers = strategy.updated_numbers(vertex);
    vertex::number_t const new_pn = numbers.first;
    vertex::number_t const new_dn = numbers.second;

    if (new_pn != vertex->proof_number || new_dn != vertex->disproof_number) {
      vertex->proof_number = new_pn;
      vertex->disproof_number = new_dn;

      for (vertex::parent_iterator parent = vertex->parents_begin(); parent != vertex->parents_end(); ++parent) {
        vertex_ptr p = *parent;
        assert(p);
        stack.push(p);
      }

    } else {
      // There's no update, and so continuing with this path to root is pointless. Quit it and consider other paths.
      continue;
    }
  }
}

template <typename S, typename H>
void pn_search_algo<S, H>::use_transposition_table(std::size_t elements, std::size_t keep_time) {
  if (elements > 0) {
    trans_tbl = transposition_table_t::create(elements, keep_time);
  } else {
    trans_tbl.reset();
  }
}

template class pn_search_algo<win_strategy, zobrist_hasher>;

#endif
