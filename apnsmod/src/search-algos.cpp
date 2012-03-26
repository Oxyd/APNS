#include "search-algos.hpp"
#include "board.hpp"
#include "movement.hpp"
#include "tree.hpp"

#include <boost/integer.hpp>

#include <cassert>
#include <limits>

namespace {

//! Check whether the game would be lost if the given player made the given step from the given position assuming the passed-in
//! game history.
bool repetition(apns::board const& board, apns::piece::color_t player, apns::history_stack::records_cont const& history) {
  using namespace apns;

  bool lose = false;

  if (!history.empty()) {
    if (history.back().player == player && history.back().position == board)
      lose = true;  // Lead doesn't move to a net change in overall position.
    else {
      // Check for third-time repetitions.
      unsigned count = 0;
      for (history_stack::records_cont::const_iterator h = history.begin(); h != history.end(); ++h)
        if (h->player == player && h->position == board)
          ++count;

      if (count >= 3)
        lose = true;
    }
  }

  return lose;
}

} // anonymous namespace

namespace apns {

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


vertex::children_iterator best_successor(vertex& parent) {
  vertex::number_t vertex::*  number      = parent.type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number;
  vertex::number_t            min_value   = vertex::max_num;
  vertex::children_iterator   min_vertex  = parent.children_end();

  for (vertex::children_iterator child = parent.children_begin(); child != parent.children_end(); ++child)
    if (child->*number < min_value || (min_vertex != parent.children_end()
                                       && min_vertex->type != parent.type 
                                       && child->*number == min_value && child->type == parent.type)) {
      min_value = child->*number;
      min_vertex = child;
    }

  return min_vertex;
}

std::pair<vertex::children_iterator, vertex::children_iterator> two_best_successors(vertex& parent) {
  vertex::number_t vertex::*  number            = parent.type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number;
  vertex::number_t            min_value         = vertex::max_num;
  vertex::number_t            second_min_value  = vertex::max_num;
  vertex::children_iterator   min_vertex        = parent.children_end();
  vertex::children_iterator   second_min_vertex = parent.children_end();

  for (vertex::children_iterator child = parent.children_begin(); child != parent.children_end(); ++child) {
    if (child->*number < min_value || (min_vertex != parent.children_end()
                                       && min_vertex->type != parent.type 
                                       && child->*number == min_value && child->type == parent.type)) {
      if (min_value < second_min_value) {
        second_min_vertex = min_vertex;
        second_min_value = min_value;
      }

      min_value = child->*number;
      min_vertex = child;
    } else if (child->*number < second_min_value || (second_min_vertex != parent.children_end()
                                                     && second_min_vertex->type != parent.type
                                                     && child->*number == second_min_value && child->type == parent.type)) {
      second_min_value = child->*number;
      second_min_vertex = child;
    }
  }

  return std::make_pair(min_vertex, second_min_vertex);
}

history_stack::history_stack(piece::color_t attacker) :
  last_visited_type(vertex::type_and),  // Root's type is always OR, so this will force push() below to add the root to history.
  attacker(attacker)
{ }

void history_stack::push(vertex const& v, board const& board) {
  if (v.type != last_visited_type)
    current_records.push_back(record(board, v.type == vertex::type_or ? attacker : opponent_color(attacker)));

  last_visited_type = v.type;
}

void history_stack::pop(vertex const& v) {
  if (v.type != last_visited_type)
    current_records.pop_back();

  last_visited_type = v.type;
}

void hashes_stack::push(vertex const& v) {
  if (v.step) {  // Unless this is the root.
    zobrist_hasher::hash_t const  last_hash           = stack.back();
    vertex::e_type const          last_visited_type   = last_visited.top().first;
    piece::color_t const          last_visited_player = last_visited.top().second;

    piece::color_t const next_player = v.type == last_visited_type ? last_visited_player : opponent_color(last_visited_player);
    zobrist_hasher::hash_t hash = hasher->update(
      last_hash, v.step->step_sequence_begin(), v.step->step_sequence_end(),
      last_visited_player == next_player ? v.steps_remaining + v.step->steps_used() : MAX_STEPS,
      v.steps_remaining,
      last_visited_player, next_player
    );
    stack.push_back(hash);
    last_visited.push(std::make_pair(v.type, next_player));
  }
}

void hashes_stack::pop() {
  stack.pop_back();
  last_visited.pop();
}

void update_numbers(vertex& v) {
  vertex::number_t vertex::* minimise_num;  // The number to take the min of.
  vertex::number_t vertex::* sum_num;       // The number to take the sum of.

  if (v.type == vertex::type_or) {
    minimise_num  = &vertex::proof_number;
    sum_num       = &vertex::disproof_number;
  } else {
    minimise_num  = &vertex::disproof_number;
    sum_num       = &vertex::proof_number;
  }

  // A number type that can hold values as large as 2 * vertex::max. This will be used to detect overflows.
  typedef boost::uint_value_t<8589934590>::fast sum_num_t;  // This magic constant sucks. But C++03 lacks constexpr...

  vertex::number_t  min = vertex::infty;
  sum_num_t         sum = 0;

  for (vertex::children_iterator child = v.children_begin(); child != v.children_end(); ++child) {
    if (child->*minimise_num < min)
      min = child->*minimise_num;

    sum += child->*sum_num;
    if (sum > vertex::infty)
      sum = vertex::infty;

    assert(sum <= vertex::infty);
  }

  if (v.type == vertex::type_or) {
    v.proof_number    = min;
    v.disproof_number = sum;
  } else {
    v.proof_number    = sum;
    v.disproof_number = min;
  }

  if (v.proof_number == 0)
    v.disproof_number = vertex::infty;
  else if (v.disproof_number == 0)
    v.proof_number = vertex::infty;

  assert(v.proof_number <= vertex::infty);
  assert(v.disproof_number <= vertex::infty);
  assert(v.proof_number < vertex::infty || v.disproof_number == 0);
  assert(v.disproof_number < vertex::infty || v.proof_number == 0);
  assert(v.proof_number != 0 || v.disproof_number == vertex::infty);
  assert(v.disproof_number != 0 || v.proof_number == vertex::infty);
}

void expand(vertex::children_iterator leaf, board_stack& state, piece::color_t attacker, transposition_table* trans_tbl, 
            hashes_stack& hashes, history_stack::records_cont const& history) {
  assert(leaf->children_count() == 0);  // leaf is a leaf.
  assert(leaf->proof_number != 0 || leaf->disproof_number != 0); // It's not (dis-)proven yet.

  // Who plays in this leaf?
  piece::color_t const player = leaf->type == vertex::type_or ? attacker : opponent_color(attacker);

  // Make a list of all possible steps.
  typedef std::vector<std::pair<step, vertex::e_type> > steps_seq;
  static steps_seq steps;  // Static so that that we don't keep allocating and reallocating it all the time.
  steps.erase(steps.begin(), steps.end());

  for (all_steps_iter new_step = all_steps_begin(state.top(), player); new_step != all_steps_end(); ++new_step)
    if (leaf->steps_remaining - static_cast<signed>(new_step->steps_used()) >= 0) {
      steps.push_back(std::make_pair(*new_step, opposite_type(leaf->type)));
      if (leaf->steps_remaining - static_cast<signed>(new_step->steps_used()) >= 1)
        steps.push_back(std::make_pair(*new_step, leaf->type));
    }

  // Attach them to the leaf now.
  leaf->reserve(steps.size());

  for (steps_seq::iterator s = steps.begin(); s != steps.end(); ++s) {
    step const&           step = s->first;
    vertex::e_type const  type = s->second;

    vertex::children_iterator child = leaf->add_child();
    child->step = step;
    child->type = type;

    if (type == leaf->type)
      child->steps_remaining = leaf->steps_remaining - static_cast<signed>(step.steps_used());
    else
      child->steps_remaining = MAX_STEPS;

    if (trans_tbl) {
      hashes.push(*child);
      boost::optional<transposition_table::entry_t> values = trans_tbl->query(hashes.top());
      hashes.pop();

      if (values) {
        vertex::number_t pn = values->first;
        vertex::number_t dn = values->second;
        assert(pn != 0 && dn != 0);

        child->proof_number = pn;
        child->disproof_number = dn;

        continue;
      }
    }

    // If values were not found in the transposition table.

    state.push(step);
    
    boost::optional<piece::color_t> winner;
    if (child->type != leaf->type)  // Only check for win if this is the start of a move.
      winner = apns::winner(state.top(), player);

    if (winner) {
      if (*winner == attacker) {
        child->proof_number     = 0;
        child->disproof_number  = vertex::infty;
      } else {
        child->proof_number     = vertex::infty;
        child->disproof_number  = 0;
      }
    } else {
      // Check for repetitions.
      if (type == leaf->type || !repetition(state.top(), player, history)) {
        child->proof_number    = 1;
        child->disproof_number = 1;
      } else {
        child->proof_number    = player == attacker ? vertex::infty : 0;
        child->disproof_number = player == attacker ? 0 : vertex::infty;
      }
    }

    state.pop(step);
  }
}

void proof_number_search::do_iterate() {
  assert(game);
  assert(!game->root.step);

  hash_visitor    hash_v(hasher, initial_hash, game->attacker);
  path_visitor    path_v;
  history_visitor history_v(game->attacker);
  board_visitor<
    boost::reference_wrapper<history_visitor>
  > board_v(game->initial_state, boost::ref(history_v));
  vertex::children_iterator leaf = traverse(
    game->root,
    &best_successor,
    make_composite_visitor(
      boost::ref(hash_v),
      make_composite_visitor(
        boost::ref(path_v),
        boost::ref(board_v))));
  
  assert(hash_v.hashes().size() == path_v.path.size());

  expand(leaf, board_v.get_board_stack(), hash_v.stack(), history_v.get_history());
  position_count += leaf->children_count();

  update_numbers(path_v.path.rbegin(), path_v.path.rend(), hash_v.hashes().rbegin());
}

void depth_first_pns::do_iterate() {
  assert(game);
  assert(!game->root.step);

  assert(path.back()->children_count() == 0);
  assert(path.back()->proof_number <= limits.back().pn_limit);
  assert(path.back()->disproof_number <= limits.back().dn_limit);

  assert(hashes.hashes().size() == path.size());
  assert(limits.size() == path.size());

  expand(path.back(), boards, hashes, history.records());
  position_count += path.back()->children_count();

  assert(path.size() == hashes.hashes().size());
  update_numbers(path.rbegin(), path.rend(), hashes.hashes().rbegin());

  // Go up.
  while (path.back()->proof_number > limits.back().pn_limit || path.back()->disproof_number > limits.back().dn_limit) {
    position_count -= path.back()->children_count();
    path.back()->resize(0);

    history.pop(*path.back());
    hashes.pop();
    boards.pop(*path.back()->step);

    path.pop_back();
    limits.pop_back();
  }

  // And go back down.
  while (path.back()->children_count() > 0) {
    std::pair<vertex::children_iterator, vertex::children_iterator> best_two = two_best_successors(*path.back());
    limits_t new_limits = make_limits(
      *best_two.first,
      *path.back(),
      best_two.second ? boost::optional<vertex&>(*best_two.second) : boost::none,
      limits.back()
    );

    limits.push_back(new_limits);
    path.push_back(best_two.first);

    assert(path.back()->proof_number <= limits.back().pn_limit && path.back()->disproof_number <= limits.back().dn_limit);

    boards.push(*path.back()->step);
    history.push(*path.back(), boards.top());
    hashes.push(*path.back());
  }
}

depth_first_pns::limits_t depth_first_pns::make_limits(vertex& v, vertex& parent, boost::optional<vertex&> second_best,
                                                       limits_t parent_limits) {
  vertex::number_t vertex::*    min_num = parent.type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number;
  vertex::number_t limits_t::*  min_lim = parent.type == vertex::type_or ? &limits_t::pn_limit : &limits_t::dn_limit;
  vertex::number_t vertex::*    dif_num = parent.type == vertex::type_or ? &vertex::disproof_number : &vertex::proof_number;
  vertex::number_t limits_t::*  dif_lim = parent.type == vertex::type_or ? &limits_t::dn_limit : &limits_t::pn_limit;

  limits_t new_limits;

  vertex::number_t second_num = second_best ? (*second_best).*min_num : vertex::infty;
  new_limits.*min_lim = std::min(parent_limits.*min_lim, second_num);

  if (parent_limits.*dif_lim < vertex::infty)
    new_limits.*dif_lim = parent_limits.*dif_lim - parent.*dif_num + v.*dif_num;
  else
    new_limits.*dif_lim = vertex::infty;

  return new_limits;
}

} // namespace apns

