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

vertex::vertex()
  : proof_number(0)
  , disproof_number(0)
  , hash(0)
  , steps_remaining(0)
  , parent(0)
  , children_size(0)
{ }

vertex::children_iterator vertex::children_begin() {
  return children.get();
}

vertex::children_iterator vertex::children_end() {
  return children.get() + children_size;
}

vertex::const_children_iterator vertex::children_begin() const {
  return children.get();
}

vertex::const_children_iterator vertex::children_end() const {
  return children.get() + children_size;
}

vertex::reverse_children_iterator vertex::children_rbegin() {
  return reverse_children_iterator(children_end());
}

vertex::reverse_children_iterator vertex::children_rend() {
  return reverse_children_iterator(children_begin());
}

vertex::const_reverse_children_iterator vertex::children_rbegin() const {
  return const_reverse_children_iterator(children_end());
}

vertex::const_reverse_children_iterator vertex::children_rend() const {
  return const_reverse_children_iterator(children_begin());
}

vertex* vertex::get_parent() {
  return parent;
}

void vertex::alloc_children(std::size_t count) {
  assert(!children.get());
  children_size = count;
  children.reset(new vertex[count]);
}

void vertex::dealloc_children() {
  children_size = 0;
  children.reset();
}

void vertex::set_parent(vertex* new_parent) {
  assert(parent == 0);
  parent = new_parent;
}

vertex* find_min(vertex::children_iterator begin, vertex::children_iterator end, vertex::number_t vertex::*number) {
  vertex::number_t min = vertex::max_num;
  vertex* min_child = 0;

  for (; begin != end; ++begin) {
    if (begin->*number < min) {
      min = begin->*number;
      min_child = begin;
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

    vertex = vertex->get_parent();
    if (vertex) {
      s = vertex->leading_step;
    }
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
    if (child->*minimise_num < min) {
      min = child->*minimise_num;
    }

    add_sum(sum, child->*add_num);
  }

  vertex::number_t const pn = vertex->type == vertex::type_or ? min : sum;
  vertex::number_t const dn = vertex->type == vertex::type_or ? sum : min;

  return std::make_pair(pn, dn);
}

pn_dn_pair_t win_strategy::initial_numbers(board const& board,
    piece::color_t initial_player, piece::color_t from_player, piece::color_t to_player) const {
  boost::optional<piece::color_t> winner;
  if (from_player != to_player) {
    winner = ::winner(board, from_player);
  }

  if (winner && *winner == initial_player) {
    return std::make_pair(0, vertex::infty);
  } else if (winner) {
    return std::make_pair(vertex::infty, 0);
  } else {
    return std::make_pair(1, 1);
  }
}
