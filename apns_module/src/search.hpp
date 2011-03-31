#ifndef SEARCH_HPP
#define SEARCH_HPP

#include "board.hpp"
#include "movement.hpp"
#include "hash.hpp"

#include <boost/pool/pool_alloc.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/utility.hpp>
#include <boost/timer.hpp>
#include <boost/python.hpp>

#include <list>
#include <vector>
#include <limits>
#include <stack>
#include <utility>
#include <stdexcept>

#include <iostream>

class vertex;

typedef boost::shared_ptr<vertex> vertex_ptr;
typedef boost::weak_ptr<vertex> weak_vertex_ptr;

/**
 * One vertex of the search tree.
 *
 * It uses a pool allocator, so creation of a vertex is only possible through the create static member function which will
 * take care of proper allocation. Deallocation is taken care of automatically by boost::shared_ptr's destructor.
 */
class vertex {
public:
  //! Type for a list of vertices.
  typedef std::vector<
    vertex_ptr
  > vertex_list;

  //! Weak list of vertices.
  typedef std::list<weak_vertex_ptr> weak_vertex_list;
  typedef unsigned number_t;                  //!< Type of proof and disproof numbers.

  static number_t const infty;    //!< Infinity value.
  static number_t const max_num;  //!< Maximum value of number_t.

  //! Vertex type.
  enum e_type {
    type_and,
    type_or
  } type;

  vertex_list children;  //!< List of children of either type of this vertex.
  weak_vertex_list parents;   //!< List of parents of this vertex.

  void add_child(vertex_ptr child);
  vertex_list::const_iterator children_begin() const;
  vertex_list::const_iterator children_end() const;

  weak_vertex_list::const_iterator parents_begin() const;
  weak_vertex_list::const_iterator parents_end() const;

  //! Which step lead to this vertex. For all non-root vertices this member must be nonempty.
  boost::optional<step> leading_step;
  unsigned steps_remaining;  //!< Number of remaining steps until the end of move.

  number_t proof_number;
  number_t disproof_number;

  unsigned pickle_number;  //!< Helper value using during saving and loading the tree from disk. Must be initialised to 0.
  bool     pickled;

  //! Create a vertex.
  static vertex_ptr create();
  //! Create and initialise a vertex.
  static vertex_ptr create(e_type type, step const& leading_step, unsigned steps_remaining);

private:
  weak_vertex_ptr self;

  vertex();  //!< Users are forbidden to create their own objects of this type directly.
};


//! Given a type of one vertex (AND or OR vertex), return the opposite type.
vertex::e_type opposite_type(vertex::e_type to_what);

//! Pair of proof and disproof number.
typedef std::pair<vertex::number_t, vertex::number_t> pn_dn_pair_t;

template <typename Strategy, typename Hasher>
class pn_search_algo_pickle;

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
  pn_search_algo(board const& initial_board, int player, Strategy strategy);
  pn_search_algo(board const& initial_board, vertex_ptr tree, piece::color_t player, Strategy strategy, unsigned position_count);

  void run(unsigned ms_how_long);
  bool finished() const;

  void iterate();

  vertex* get_root() const                { return root.get(); }
  board const& get_initial_board() const  { return initial_board; }
  piece::color_t get_player() const       { return player; }
  std::size_t get_position_count() const  { return position_count; }
  Strategy const& get_strategy() const    { return strategy; }
  vertex_ptr successor(vertex_ptr node);

  static std::size_t get_size_of_trans_tbl_element() { return transposition_table_t::SIZE_OF_ELEMENT; }
  void use_transposition_table(std::size_t elements, std::size_t keep_time);
  transposition_table_t const* get_transposition_table() const { return trans_tbl.get(); }

private:
  friend class pn_search_algo_pickle<Strategy, Hasher>;

  typedef typename Hasher::hash_t hash_t;

  Hasher                hasher;
  Strategy              strategy;
  vertex_ptr            root;
  board const&          initial_board;
  hash_t                initial_hash;
  piece::color_t        player;
  trans_tbl_ptr         trans_tbl;
  std::size_t           position_count;

  void find_leaf(board& board, vertex_ptr root, vertex_ptr& leaf, hash_t& leaf_hash);
  void insert(vertex_ptr what, vertex_ptr parent);
  void expand(board& board, vertex_ptr leaf, hash_t leaf_hash);
  void generate_steps(board& board, vertex_ptr leaf, hash_t leaf_hash,
      piece::color_t from_player, piece::color_t to_player, vertex::e_type type, unsigned steps_remaining);
  void update_numbers(vertex_ptr leaf);
};

struct win_strategy {
  pn_dn_pair_t updated_numbers(vertex_ptr vertex) const;
  pn_dn_pair_t initial_numbers(board const& board, vertex::e_type type, piece::color_t player) const;
};

namespace detail {

void add_sum(vertex::number_t& sum, vertex::number_t addend);
vertex::number_t add(vertex::number_t first, vertex::number_t second);
void add_sum_no_infty(vertex::number_t& sum, vertex::number_t addend);

}

template <typename Strategy, typename Hasher>
pn_search_algo<Strategy, Hasher>::pn_search_algo(board const& initial_board, piece::color_t player, Strategy strategy)
  : strategy(strategy)
  , root(vertex::create())
  , initial_board(initial_board)
  , initial_hash(hasher.generate_initial(initial_board, player))
  , player(player)
  , position_count(1)
{
  root->type = vertex::type_or;
  pn_dn_pair_t pn_dn = strategy.initial_numbers(initial_board, vertex::type_or, player);
  root->proof_number = pn_dn.first;
  root->disproof_number = pn_dn.second;
  root->steps_remaining = MAX_STEPS;
}

template <typename Strategy, typename Hasher>
pn_search_algo<Strategy, Hasher>::pn_search_algo(board const& initial_board, int player, Strategy strategy)
  : strategy(strategy)
  , root(vertex::create())
  , initial_board(initial_board)
  , initial_hash(hasher.generate_initial(initial_board, color_from_int(player)))
  , player(color_from_int(player))
  , position_count(1)
{
  root->type = vertex::type_or;
  pn_dn_pair_t pn_dn = strategy.initial_numbers(initial_board, vertex::type_or, this->player);
  root->proof_number = pn_dn.first;
  root->disproof_number = pn_dn.second;
  root->steps_remaining = MAX_STEPS;
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

  vertex_ptr leaf;
  hash_t leaf_hash = initial_hash;
  find_leaf(board, root, leaf, leaf_hash);
  expand(board, leaf, leaf_hash);
  update_numbers(leaf);
}

template <typename Strategy, typename H>
void pn_search_algo<Strategy, H>::find_leaf(board& board, vertex_ptr root, vertex_ptr& leaf, hash_t& leaf_hash) {
  vertex_ptr previous;
  vertex_ptr current = root;

  piece::color_t last_player = player;
  do {
    assert((current->proof_number != 0 && current->disproof_number != 0) || current == root);
    previous = current;
    current = successor(current);

    if (previous->leading_step) {
      apply(*previous->leading_step, board);

      piece::color_t const current_player = previous->type == vertex::type_or ? player : opponent_color(player);

      leaf_hash = hasher.update(leaf_hash,
          previous->leading_step->step_sequence_begin(), previous->leading_step->step_sequence_end(),
          last_player, current_player);

      last_player = current_player;
    } else if (previous != root) {
      throw std::logic_error("Non-root vertex without leading step defined.");
    }
  } while (current);

  leaf = previous;

  assert(leaf->children_begin() == leaf->children_end());
}

template <typename Strategy, typename H>
void pn_search_algo<Strategy, H>::insert(vertex_ptr what, vertex_ptr parent) {
  assert(parent);
  assert(what);

  parent->children.insert(parent->children.end(), what);
  what->parents.insert(what->parents.end(), parent);
}

template <typename Strategy, typename H>
void pn_search_algo<Strategy, H>::expand(board& board, vertex_ptr leaf, hash_t leaf_hash) {
  assert(leaf->children.empty());
  assert(!(leaf->proof_number == 0 && leaf->disproof_number == 0));

  piece::color_t const player = (leaf->type == vertex::type_or ? this->player : opponent_color(this->player));

  if (leaf->steps_remaining > 0) {
    // Generate steps by the same player.
    generate_steps(board, leaf, leaf_hash, player, player, leaf->type, leaf->steps_remaining);
  }

  // Generate opponent's steps.
  if (leaf->steps_remaining != MAX_STEPS) {
    generate_steps(board, leaf, leaf_hash, player, opponent_color(player), opposite_type(leaf->type), MAX_STEPS);
  }
}

template <typename Strategy, typename Hash>
void pn_search_algo<Strategy, Hash>::generate_steps(board& board, vertex_ptr leaf, hash_t leaf_hash,
    piece::color_t from_player, piece::color_t to_player, vertex::e_type type, unsigned steps_remaining) {
  for (all_steps_iter step = all_steps_begin(board, to_player); step != all_steps_end(); ++step) {
    if (steps_remaining >= step->steps_used()) {  // Would not the complete move be too long?
      ::step s = *step;
      hash_t child_hash = hasher.update(leaf_hash, s.step_sequence_begin(), s.step_sequence_end(), from_player, to_player);

      vertex_ptr new_vertex;
      if (trans_tbl) {
        new_vertex = trans_tbl->query(child_hash);

        if (new_vertex && new_vertex->leading_step->to_string() == s.to_string()) {
          if (new_vertex->steps_remaining == leaf->steps_remaining - s.steps_used()) {
            // We've got a perfect match. Just use the cached vertex.
            insert(new_vertex, leaf);
            continue;
          }
        }
      }

      // If the vertex wasn't found, or the number of remaining steps doesn't match, create a new one.
      new_vertex = vertex::create(type, s, steps_remaining - step->steps_used());

      apply(s, board);

      pn_dn_pair_t const numbers = strategy.initial_numbers(board, type, to_player);
      new_vertex->proof_number = numbers.first;
      new_vertex->disproof_number = numbers.second;

      unapply(s, board);

      ++position_count;
      insert(new_vertex, leaf);

      if (trans_tbl) {
        trans_tbl->insert(child_hash, new_vertex);
      }
    }
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

      for (vertex::weak_vertex_list::const_iterator parent = vertex->parents.begin(); parent != vertex->parents.end(); ++parent) {
        vertex_ptr p = parent->lock();
        assert(p);
        stack.push(p);
      }

    } else {
      // There's no update, and so continuing with this path to root is pointless. Quit it and consider other paths.
      continue;
    }
  }
}

template <typename Strategy, typename H>
vertex_ptr pn_search_algo<Strategy, H>::successor(vertex_ptr node) {
  using detail::add_sum;
  using detail::add_sum_no_infty;
  using detail::add;

  assert(node);

  // To prove an OR vertex, one needs to either prove one OR child or all AND children. To disprove an OR vertex, one needs to
  // disprove all OR children and at least one AND child. So -- we'll keep track of how much work is needed to:
  //   -- prove one OR child
  //   -- prove all AND children
  //   -- disprove all OR children
  //   -- disprove one AND child
  // Then we'll compare these values and decide which child is the best.
  // AND vertices are treated in a similar fashion.

  // For an OR node, num1 is PN, num2 is DN; for an AND it's the other way around.
  vertex::number_t const vertex::*num1;
  vertex::number_t const vertex::*num2;

  if (node->type == vertex::type_or) {
    num1 = &vertex::proof_number;
    num2 = &vertex::disproof_number;
  } else {
    num1 = &vertex::disproof_number;
    num2 = &vertex::proof_number;
  }

  vertex::number_t min_num1_same = vertex::max_num;
  vertex::number_t sum_num2_same = 0;
  vertex::number_t min_num2_opponent = vertex::max_num;
  vertex::number_t sum_num1_opponent = 0;
  vertex::number_t min_num1_opponent = vertex::max_num;
  vertex::number_t min_num2_same = vertex::max_num;

  vertex_ptr min_num1_same_child;
  vertex_ptr min_num1_opponent_child;
  vertex_ptr min_num2_same_child;
  vertex_ptr min_num2_opponent_child;

  for (vertex::vertex_list::const_iterator child_it = node->children_begin(); child_it != node->children_end(); ++child_it) {
    vertex_ptr const& child_ptr = *child_it;
    vertex const& child = *child_ptr;
    vertex::number_t const n1 = child.*num1;
    vertex::number_t const n2 = child.*num2;

    if (child.type == node->type) {
      if (n1 < min_num1_same) {
        min_num1_same = n1;
        min_num1_same_child = child_ptr;
      }

      if (0 < n2 && n2 < min_num2_same) {
        min_num2_same = n2;
        min_num2_same_child = child_ptr;
      }

      add_sum_no_infty(sum_num2_same, n2);

    } else {
      if (n2 < min_num2_opponent) {
        min_num2_opponent = n2;
        min_num2_opponent_child = child_ptr;
      }

      if (0 < n1 && n1 < min_num1_opponent) {
        min_num1_opponent = n1;
        min_num1_opponent_child = child_ptr;
      }

      add_sum(sum_num1_opponent, n1);
    }
  }

  // So, which option is the best, then?
  if (min_num1_same_child && min_num1_same <= sum_num1_opponent && min_num1_same <= add(sum_num2_same, min_num2_opponent)) {
    return min_num1_same_child;
  } else if (min_num1_opponent_child && min_num1_opponent <= min_num1_same && min_num1_opponent <= add(sum_num2_same, min_num2_opponent)) {
    return min_num1_opponent_child;
  } else if (min_num2_same_child && min_num2_opponent_child && add(sum_num2_same, min_num2_opponent) <= min_num1_opponent
      && add(sum_num2_same, min_num2_opponent) <= min_num1_same) {
    if (min_num2_same_child.get()->*num2 < min_num2_opponent_child.get()->*num2) {
      return min_num2_same_child;
    } else {
      return min_num2_opponent_child;
    }
  } else if (!node->leading_step) {  // Root vertex needs a special treatement as it has no AND children.
    return min_num1_same_child;
  } else {
    return vertex_ptr();
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
