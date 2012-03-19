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
bool repetition(board& board, piece::color_t player, history_visitor::history_cont const& history) {
  bool lose = false;

  if (!history.empty()) {
    if (history.back().position == board && history.back().player == player)
      lose = true;  // Lead doesn't move to a net change in overall position.
    else {
      // Check for third-time repetitions.
      unsigned count = 0;
      for (history_visitor::history_cont::const_iterator h = history.begin(); h != history.end(); ++h)
        if (h->player == player && h->position == board)
          ++count;

      if (count >= 3)
        lose = true;
    }
  }

  return lose;
}

} // anonymous namespace

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
  vertex::number_t vertex::* number = parent.type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number;
  vertex::number_t min_value = vertex::max_num;
  vertex*          min_vertex = 0;

  for (vertex::children_iterator child = parent.children_begin(); child != parent.children_end(); ++child)
    if (child->*number < min_value) {
      min_value = child->*number;
      min_vertex = &*child;
    }

  return min_vertex;
}

history_visitor::history_visitor(piece::color_t attacker) :
  last_visited_type(vertex::type_and),  // Root's type is always OR, so this will force op () below to add the root to history.
  attacker(attacker)
{ }

bool history_visitor::operator () (vertex const& v, board const& board) {
  if (v.type != last_visited_type)
    history.push_back(history_record(board, v.type == vertex::type_or ? attacker : opponent_color(attacker)));

  last_visited_type = v.type;

  return false;
}

bool hash_visitor::operator () (vertex const& v) {
  if (v.step) {  // Unless this is the root.
    zobrist_hasher::hash_t last_hash = hashes.back();
    piece::color_t const next_player = v.type == last_visited_type ? last_visited_player : opponent_color(last_visited_player);
    zobrist_hasher::hash_t hash = hasher->update(last_hash, v.step->step_sequence_begin(), v.step->step_sequence_end(),
                                                 last_visited_player, next_player);
    hashes.push_back(hash);

    last_visited_player = next_player;
    last_visited_type   = v.type;
  }

  return false;
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

void proof_number_search::do_iterate() {
  assert(game);
  assert(!game->root.step);

  if (finished()) return;

  if (trans_tbl)
    trans_tbl->tick();

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
  
  assert(hash_v.hashes.size() == path_v.path.size());

  expand(leaf, board_v.board, hash_v.hashes.back(), history_v.history);

  if (trans_tbl && leaf->proof_number != 0 && leaf->disproof_number != 0)
    trans_tbl->insert(hash_v.hashes.back(), std::make_pair(leaf->proof_number, leaf->disproof_number));

  hash_visitor::hashes_cont::const_reverse_iterator hash = hash_v.hashes.rbegin();
  for (path_visitor::path_cont::reverse_iterator vertex = path_v.path.rbegin();
       vertex != path_v.path.rend();
       ++vertex, ++hash) {
    update_numbers(**vertex);
    if (trans_tbl && vertex != path_v.path.rbegin() && (*vertex)->proof_number != 0 && (*vertex)->disproof_number != 0)
      trans_tbl->update(*hash, std::make_pair((*vertex)->proof_number, (*vertex)->disproof_number));
  }
}

void proof_number_search::expand(vertex::children_iterator leaf, ::board& leaf_state,
                                 zobrist_hasher::hash_t leaf_hash, history_visitor::history_cont const& history) {
  assert(leaf->children_count() == 0);  // leaf is a leaf.
  assert(leaf->proof_number != 0 || leaf->disproof_number != 0); // It's not (dis-)proven yet.

  // Who plays in this leaf?
  piece::color_t const player = leaf->type == vertex::type_or ? game->attacker : opponent_color(game->attacker);
  piece::color_t const opponent = opponent_color(player);

  // Make a list of all possible steps.
  typedef std::vector<std::pair<step, vertex::e_type> > steps_seq;
  steps_seq steps;

  for (all_steps_iter new_step = all_steps_begin(leaf_state, player); new_step != all_steps_end(); ++new_step)
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
    ++position_count;
    child->step = step;
    child->type = type;

    if (type == leaf->type)
      child->steps_remaining = leaf->steps_remaining - static_cast<signed>(step.steps_used());
    else
      child->steps_remaining = MAX_STEPS;

    if (trans_tbl) {
      zobrist_hasher::hash_t child_hash = hasher.update(
        leaf_hash, child->step->step_sequence_begin(), child->step->step_sequence_end(),
        player, leaf->type == child->type ? player : opponent
      );
      boost::optional<transposition_table::entry_t> values = trans_tbl->query(child_hash);
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

    apply(step, leaf_state);
    
    boost::optional<piece::color_t> winner;
    if (child->type != leaf->type)  // Only check for win if this is the start of a move.
      winner = ::winner(leaf_state, player);

    if (winner) {
      if (*winner == game->attacker) {
        child->proof_number     = 0;
        child->disproof_number  = vertex::infty;
      } else {
        child->proof_number     = vertex::infty;
        child->disproof_number  = 0;
      }
    } else {
      // Check for repetitions.
      if (type == leaf->type || !repetition(leaf_state, player, history)) {
        child->proof_number    = 1;
        child->disproof_number = 1;
      } else {
        child->proof_number    = player == game->attacker ? vertex::infty : 0;
        child->disproof_number = player == game->attacker ? 0 : vertex::infty;
      }
    }

    unapply(step, leaf_state);
  }
}

