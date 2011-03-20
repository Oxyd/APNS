#include "search.hpp"

#include "board.hpp"

#include <boost/pool/pool_alloc.hpp>
#include <boost/shared_ptr.hpp>

#include <limits>
#include <cassert>

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

void vertex::add_child(vertex_ptr child) {
  children.insert(children.end(), child);
  child->parents.insert(child->parents.end(), self);
}

vertex::vertex_list::const_iterator vertex::children_begin() const {
  return children.begin();
}

vertex::vertex_list::const_iterator vertex::children_end() const {
  return children.end();
}

vertex::weak_vertex_list::const_iterator vertex::parents_begin() const {
  return parents.begin();
}

vertex::weak_vertex_list::const_iterator vertex::parents_end() const {
  return parents.end();
}

vertex_ptr vertex::create() {
  vertex* storage = vertex::allocator::allocate();
  vertex* v = 0;

  try {
    v = new (storage) vertex;
  } catch (...) {
    vertex::allocator::deallocate(storage);
    throw;
  }

  vertex_ptr result = boost::shared_ptr<vertex>(v, &vertex::destroy);
  result->self = result;
  return result;
}

vertex_ptr vertex::create(e_type type, step const& leading_step, unsigned steps_remaining) {
  vertex_ptr v = vertex::create();
  v->type = type;
  v->leading_step = leading_step;
  v->steps_remaining = steps_remaining;

  return v;
}

vertex::vertex() : pickle_number(0), pickled(false) { }

void vertex::destroy(vertex* ptr) {
  ptr->vertex::~vertex();
  vertex::allocator::deallocate(ptr);
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

pn_dn_pair_t win_strategy::updated_numbers(vertex_ptr vertex) const {
  using detail::add_sum;
  using detail::add;
  using detail::add_sum_no_infty;

  // To prove an OR vertex, we need to either prove one OR child (that is, find one good step for the same player) or prove
  // *all* AND children (that is, give it over to the opponent and prove that the opponent necessarily loses every time).
  // Therefore:
  //     PN := min { min_{c is an OR child} c.PN, sum_{c is an AND child} c.PN } .
  //
  // To disprove this vertex, we need to show that all possible moves of the player from this position result in the player's
  // loss and that the opponent has at least one winning option. So:
  //     DN := min_{c is an AND child} c.DN + sum_{c is an OR child & c.DN =/= oo} c.DN .
  //
  // To prove an AND vertex -- that is, show that the opponent loses no matter what move he choses -- means to prove all
  // his possible moves -- AND children -- and at least one player's move.
  //     PN := min_{c is an OR child} c.PN + sum_{c is an AND child} c.PN
  //
  // To disprove this vertex one needs to either find a good AND child that will lead to opponent's victory, or show
  // that all OR successors are disproved. So:
  //     DN := min { min_{c is an AND child} c.DN, sum_{c is an OR child & c.DN =/= oo} c.DN }
  //
  // We don't sum up infinities in the two sums, because children with PN/DN = oo are already disproved and nothing needs to be
  // done about them.

  vertex::number_t vertex::*num1;
  vertex::number_t vertex::*num2;

  if (vertex->type == vertex::type_or) {
    num1 = &vertex::proof_number;
    num2 = &vertex::disproof_number;
  } else {
    num1 = &vertex::disproof_number;
    num2 = &vertex::proof_number;
  }

  vertex::number_t min_num1_same = vertex::max_num;
  vertex::number_t min_num2_opposite = vertex::max_num;
  vertex::number_t sum_num1_opposite = 0;
  vertex::number_t sum_num2_same = 0;

  if (!vertex->leading_step) {
    // Root is not going to have any AND children, but that doesn't mean the opponent is immobilised.
    sum_num1_opposite = vertex::infty;
    min_num2_opposite = 0;
  }

  for (vertex::vertex_list::const_iterator child = vertex->children_begin(); child != vertex->children_end(); ++child) {
    if ((*child)->type == vertex->type) {
      if (child->get()->*num1 < min_num1_same) {
        min_num1_same = child->get()->*num1;
      }

      add_sum(sum_num2_same, child->get()->*num2);
    } else {
      if (child->get()->*num2 < min_num2_opposite) {
        min_num2_opposite = child->get()->*num2;
      }

      add_sum(sum_num1_opposite, child->get()->*num1);
    }
  }

  if (vertex->type == vertex::type_or) {
    vertex::number_t const pn = std::min(min_num1_same, sum_num1_opposite);
    vertex::number_t const dn = add(min_num2_opposite, sum_num2_same);

    assert((pn != 0 || dn == vertex::infty) && (dn != 0 || pn == vertex::infty));

    return std::make_pair(pn, dn);

  } else {
    vertex::number_t const pn = add(min_num2_opposite, sum_num2_same);
    vertex::number_t const dn = std::min(min_num1_same, sum_num1_opposite);

    assert((pn != 0 || dn == vertex::infty) && (dn != 0 || pn == vertex::infty));

    return std::make_pair(pn, dn);
  }
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
