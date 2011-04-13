#include "search.hpp"

#include "board.hpp"

#include <boost/pool/pool_alloc.hpp>
#include <boost/shared_ptr.hpp>

#include <limits>
#include <cassert>
#include <stack>
#include <queue>
#include <iostream>

namespace {

/**
 * Did any player win?
 *
 * \param board The game situation.
 * \param player Last player to have made a move.
 * \return If any player has won, return their color; otherwise return nothing.
 */
boost::optional<piece::color_t> winner(board const& board, piece::color_t player) {
  piece::color_t const opponent = opponent_color(player);

  position::row_t players_target_row;
  position::row_t opponents_target_row;

  if (player == piece::gold) {
    players_target_row = board::MAX_ROW;
    opponents_target_row = board::MIN_ROW;
  } else {
    players_target_row = board::MIN_ROW;
    opponents_target_row = board::MAX_ROW;
  }

  bool player_goal = false;
  bool opponent_goal = false;
  bool player_has_rabbits = false;
  bool opponent_has_rabbits = false;

  for (board::pieces_iterator pos_piece = board.pieces_begin(); pos_piece != board.pieces_end(); ++pos_piece) {
    position const& position = pos_piece->first;
    piece const& piece = pos_piece->second;

    if (piece.get_type() == piece::rabbit) {
      if (piece.get_color() == player) {
        player_has_rabbits = true;

        if (position.get_row() == players_target_row) {
          player_goal = true;
          break;
        }

      } else {
        opponent_has_rabbits = true;

        if (position.get_row() == opponents_target_row) {
          opponent_goal = true;
        }
      }
    }
  }

  if (player_goal) {
    return player;
  } else if (opponent_goal) {
    return opponent;
  } else if (!opponent_has_rabbits) {
    return player;
  } else if (!player_has_rabbits) {
    return opponent;
  }

  return boost::optional<piece::color_t>();
}

}

vertex::number_t const vertex::infty = std::numeric_limits<vertex::number_t>::max() - 1;
vertex::number_t const vertex::max_num = std::numeric_limits<vertex::number_t>::max();

vertex::parent_iterator::parent_iterator(parent_node* current)
  : current(current)
{ }

void vertex::parent_iterator::increment() {
  current = current->next.get();
}

vertex::parent_iterator::reference vertex::parent_iterator::dereference() const {
  return current->parent;
}

bool vertex::parent_iterator::equal(vertex::parent_iterator const& other) const {
  return current == other.current;
}

vertex::vertex()
  : proof_number(0)
  , disproof_number(0)
  , steps_remaining(0)
  , visited(false)
  , children_size(0)
{ }

vertex::children_iterator vertex::children_begin() {
  return children.get();
}

vertex::children_iterator vertex::children_end() {
  return children.get() + children_size;
}

vertex::parent_iterator vertex::parents_begin() {
  return parent_iterator(parent_list.get());
}

vertex::parent_iterator vertex::parents_end() {
  return parent_iterator();
}

void vertex::alloc_children(std::size_t count) {
  assert(!children.get());
  children_size = count;
  children.reset(new vertex*[count]);
  std::fill_n(children.get(), children_size, static_cast<vertex*>(0));
}

void vertex::add_parent(vertex* parent) {
  parent_list.reset(new parent_node(parent, parent_list));
}

void vertex::remove_parent(vertex* parent) {
  parent_node* prev = 0;
  parent_node* current = parent_list.get();

  while (current && current->parent != parent) {
    prev = current;
    current = current->next.get();
  }

  assert(current);

  if (prev) {
    prev->next.reset(current->next.get());
  } else {
    parent_list.reset(current->next.get());
  }
}

void delete_subtree(vertex* root) try {
  std::queue<vertex*> queue;
  queue.push(root);

  while (!queue.empty()) {
    vertex* v = queue.front();
    queue.pop();

    for (vertex::children_iterator child = v->children_begin(); child != v->children_end(); ++child) {
      if (*child) {
        (*child)->remove_parent(v);

        if ((*child)->parent_list.get() == 0) {
          // This was the last parent of the child.
          queue.push(*child);
        }
      }
    }

    delete v;
  }
} catch (...) {
  std::cerr << "Error: Exception thrown during subtree deallocation. Memory leaked.\n";
  throw;
}

vertex* find_min(vertex::children_iterator begin, vertex::children_iterator end, vertex::number_t vertex::*number) {
  vertex::number_t min = vertex::max_num;
  vertex* min_child = 0;

  for (; begin != end; ++begin) {
    if ((*begin)->*number < min) {
      min = (*begin)->*number;
      min_child = *begin;
    }
  }

  return min_child;
}

vertex::e_type opposite_type(vertex::e_type to_what) {
  switch (to_what) {
  case vertex::type_and:  return vertex::type_or;
  case vertex::type_or:   return vertex::type_and;
  default: assert(!"It's gone pear-shaped!");
  }

  return vertex::type_and;  // Fix a compiler warning.
}

void detail::add_sum(vertex::number_t& sum, vertex::number_t addend) {
  if (sum < vertex::infty) {
    if (addend < vertex::infty) {
      sum += addend;
    } else {
      sum = vertex::infty;
    }
  }

  assert(sum <= vertex::infty);
}

vertex::number_t detail::add(vertex::number_t first, vertex::number_t second) {
  if (first < vertex::infty && second < vertex::infty) {
    return first + second;
  } else {
    return vertex::infty;
  }
}

void detail::add_sum_no_infty(vertex::number_t& sum, vertex::number_t addend) {
  if (addend < vertex::infty) {
    add_sum(sum, addend);
  }
}

board detail::board_from_vertex(vertex_ptr vertex, board const& initial_board) {
  std::stack<step> stack;

  boost::optional<step> s = vertex->leading_step;
  while (s) {
    stack.push(*s);

    // It doesn't matter which parent we choose here, so let's just grab the first one each time. Also, if leading step is not
    // empty, a parent must always exist.
    assert(vertex->parents_begin() != vertex->parents_end());
    vertex = vertex_ptr(*vertex->parents_begin());

    s = vertex->leading_step;
  }

  // Now, go from initial_board to the final one by going down the stack and applying the steps.
  board result = initial_board;
  while (!stack.empty()) {
    apply(stack.top(), result);
    stack.pop();
  }

  return result;
}

pn_dn_pair_t win_strategy::updated_numbers(vertex_ptr vertex) const {
  using detail::add_sum;
  using detail::add;
  using detail::add_sum_no_infty;

  vertex::number_t vertex::* minimise_num;  // The number to take the minimum of.
  vertex::number_t vertex::* add_num;       // The number to take sum of.

  if (vertex->type == vertex::type_or) {
    minimise_num = &vertex::proof_number;
    add_num = &vertex::disproof_number;
  } else {
    minimise_num = &vertex::disproof_number;
    add_num = &vertex::proof_number;
  }

  vertex::number_t min = vertex::max_num;
  vertex::number_t sum = 0;

  for (vertex::children_iterator child = vertex->children_begin(); child != vertex->children_end(); ++child) {
    assert(*child);
    if ((*child)->*minimise_num < min) {
      min = (*child)->*minimise_num;
    }

    add_sum(sum, (*child)->*add_num);
  }

  vertex::number_t const pn = vertex->type == vertex::type_or ? min : sum;
  vertex::number_t const dn = vertex->type == vertex::type_or ? sum : min;

  return std::make_pair(pn, dn);
}

pn_dn_pair_t win_strategy::initial_numbers(board const& board, vertex::e_type type, piece::color_t player) const {
  boost::optional<piece::color_t> winner = ::winner(board, player);
  if (winner && ((*winner == player && type == vertex::type_or) || (*winner != player && type == vertex::type_and))) {
    return std::make_pair(0, vertex::infty);
  } else if (winner) {
    return std::make_pair(vertex::infty, 0);
  } else {
    return std::make_pair(1, 1);
  }
}
