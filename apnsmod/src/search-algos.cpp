#include "search-algos.hpp"
#include "board.hpp"
#include "movement.hpp"
#include "tree.hpp"

#include <boost/integer.hpp>

#include <cassert>
#include <vector>

namespace {

//! Check whether the game would be lost if the given player made the given step from the given position assuming the passed-in
//! game history.
bool repetition(apns::zobrist_hasher::hash_t hash, apns::history_stack::records_cont const& history) {
  using namespace apns;

  if (!history.empty()) {
    if (history.back().hash == hash)
      return true;  // Move doesn't lead to a net change in overall position.
    else {
      // Check for third-time repetitions.
      unsigned count = 0;
      for (history_stack::records_cont::const_iterator h = history.begin(); h != history.end(); ++h) {
        if (h->hash == hash) {
          ++count;

          if (count == 3) {
            return true;
          }
        }
      }
    }
  }

  return false;
}

apns::piece::color_t vertex_player(apns::vertex const& v, apns::piece::color_t attacker) {
  return v.type == apns::vertex::type_or ? attacker : apns::opponent_color(attacker);
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
    position position = pos_piece->first;
    piece piece = pos_piece->second;

    if (piece.type() == piece::rabbit) {
      if (piece.color() == player) {
        player_has_rabbits = true;

        if (position.row() == players_target_row) {
          player_goal = true;
          break;
        }

      } else {
        opponent_has_rabbits = true;

        if (position.row() == opponents_target_row) {
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


vertex* best_successor(vertex& parent) {
  if (!parent.leaf())
    return &*parent.children_begin();
  else
    return 0;
}

std::pair<vertex*, vertex*> two_best_successors(vertex& parent) {
  if (parent.children_count() >= 2)
    return std::make_pair(&*parent.children_begin(), &*boost::next(parent.children_begin()));
  else if (parent.children_count() == 1)
    return std::make_pair(&*parent.children_begin(), static_cast<vertex*>(0));
  else
    return std::make_pair(static_cast<vertex*>(0), static_cast<vertex*>(0));
}

history_stack::history_stack() {
  last_.push(vertex::type_and);  // Root's type is always OR, so this will force push() below to add the root to history.
}

void history_stack::push(vertex const& v, zobrist_hasher::hash_t hash) {
  if (v.type != last_.top()) {
    current_records_.push_back(record(hash));
  }

  last_.push(v.type);
}

void history_stack::pop(vertex const& v) {
  last_.pop();

  if (!last_.empty()) {
    if (v.type != last_.top()) {
      current_records_.pop_back();
    }
  } else {
    assert(current_records_.empty());
    last_.push(vertex::type_and);
  }
}

void hashes_stack::push(vertex const& v) {
  if (v.step) {  // Unless this is the root.
    zobrist_hasher::hash_t const  last_hash           = stack_.back();
    vertex::e_type const          last_visited_type   = last_visited_.top().type;
    piece::color_t const          last_visited_player = last_visited_.top().player;

    piece::color_t const next_player = v.type == last_visited_type ? last_visited_player : opponent_color(last_visited_player);
    zobrist_hasher::hash_t hash = hasher_->update(
      last_hash, v.step->step_sequence_begin(), v.step->step_sequence_end(),
      last_visited_player, next_player
    );
    stack_.push_back(hash);
    last_visited_.push(last(v.type, next_player));
  }
}

void hashes_stack::pop() {
  stack_.pop_back();
  last_visited_.pop();
}

void killer_db::add(std::size_t ply, vertex::e_type type, step const& step) {
  plys& p = get_plys(type);
  if (ply >= p.size())
    p.resize(ply + 1, ply_killers_t(killer_count_));

  if (std::find(p[ply].begin(), p[ply].end(), step) == p[ply].end())
    p[ply].push_back(step);
}

bool killer_db::is_killer(std::size_t ply, vertex::e_type type, step const& step) const {
  for (ply_iterator killer = ply_begin(ply, type); killer != ply_end(ply, type); ++killer)
    if (*killer == step)
      return true;

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

  v.*minimise_num = min;
  v.*sum_num      = sum;

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

namespace detail {

void expand(vertex& leaf, board_stack& state, piece::color_t attacker,
            transposition_table* trans_tbl, proof_table* proof_tbl, 
            std::size_t ply, killer_db const& killers, hashes_stack& hashes, history_stack const& history) {
  assert(leaf.children_count() == 0);  // leaf is a leaf.
  assert(leaf.proof_number != 0 || leaf.disproof_number != 0); // It's not (dis-)proven yet.

  // Who plays in this leaf?
  piece::color_t const player = leaf.type == vertex::type_or ? attacker : opponent_color(attacker);

  // Make a list of all possible steps.
  typedef std::vector<std::pair<step, vertex::e_type> > steps_seq;
  static steps_seq steps;  // Static so that that we don't keep allocating and reallocating it all the time.
  steps.erase(steps.begin(), steps.end());

  for (all_steps_iter new_step = all_steps_begin(state.top(), player); new_step != all_steps_end(); ++new_step)
    if (leaf.steps_remaining - static_cast<signed>(new_step->steps_used()) >= 0) {
      steps.push_back(std::make_pair(*new_step, opposite_type(leaf.type)));
      if (leaf.steps_remaining - static_cast<signed>(new_step->steps_used()) >= 1)
        steps.push_back(std::make_pair(*new_step, leaf.type));
    }

  // Attach them to the leaf now.
  leaf.reserve(steps.size());
  
  vertex::children_iterator killers_end = leaf.children_begin();
  for (steps_seq::iterator s = steps.begin(); s != steps.end(); ++s) {
    step                  step = s->first;
    vertex::e_type const  type = s->second;
    vertex::children_iterator child = leaf.add_child();

    process_new(*child, leaf, attacker, step, type, trans_tbl, proof_tbl, hashes, state, history);
    assert(child->step);

    if (killers.is_killer(ply, type, step)) {
      if (child != killers_end)
        leaf.swap_children(child, killers_end);
      ++killers_end;
    }
  }

  sort_children(leaf, vertex_comparator(leaf));
}

//! Cut non-(dis-)proof children of this vertex.
//!
//! \returns Number of vertices cut.
std::size_t cut(vertex& parent) {
  if (parent.children_count() > 0 &&
      ((parent.type == vertex::type_or && parent.proof_number == 0)
       || (parent.type == vertex::type_and && parent.disproof_number == 0))) {
    vertex::number_t vertex::* num = parent.type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number;
    vertex_counter cut_counter;

    // Only one child is required as a proof here.
    vertex::children_iterator proof = parent.children_end();
    for (vertex::children_iterator child = parent.children_begin(); child != parent.children_end(); ++child)
      if (proof == parent.children_end() && child->*num == 0)
        proof = child;
      else
        traverse(*child, backtrack(), boost::ref(cut_counter));

    if (proof != parent.children_begin())
      parent.swap_children(proof, parent.children_begin());

    parent.resize(1);
    parent.pack();
    return cut_counter.count;
  }
  else if (parent.proof_number == 0 || parent.disproof_number == 0)
    // All children constitute a proof here.
    return 0;
  else {
    // No proof.
    vertex_counter counter;
    traverse(parent, backtrack(), boost::ref(counter));
    parent.resize(0);
    parent.pack();
    return counter.count - 1;  // parent has not been removed, but it's been counted.
  }
}

void process_new(vertex& child, vertex const& parent, piece::color_t attacker, step const& step, 
                 vertex::e_type type, transposition_table* trans_tbl, proof_table* proof_tbl, 
                 hashes_stack& hashes, board_stack& state, history_stack const& history) {
  piece::color_t const player = parent.type == vertex::type_or ? attacker : opponent_color(attacker);

  child.step = step;
  child.type = type;
  child.proof_number = 0;
  child.disproof_number = 0;

  if (type == parent.type)
    child.steps_remaining = parent.steps_remaining - static_cast<signed>(step.steps_used());
  else
    child.steps_remaining = MAX_STEPS;

  state.push(step);
  hashes.push(child);

  if (proof_tbl) {
    boost::optional<proof_table::entry_t> values = proof_tbl->query(hashes.top());

    if (values) {
      // Check whether the histories are compatible.
      boost::unordered_map<zobrist_hasher::hash_t, std::size_t> repetitions;

      bool repetition = false;

      for (history_t::const_iterator h = values->history.begin(); h != values->history.end(); ++h)
        ++repetitions[*h];

      for (history_stack::records_cont::const_iterator h = history.records().begin();
           h != history.records().end() && !repetition; ++h) {
        if (++repetitions[h->hash] >= 3) {
          // This would possibly (but not surely) use a third-time repetition somewhere along the path.
          repetition = true;
        }
      }

      if (!repetition) {
        vertex::number_t const pn = values->proof_number;
        vertex::number_t const dn = values->disproof_number;
        assert(pn == 0 || dn == 0);

        child.proof_number = pn;
        child.disproof_number = dn;
      }
      else {
        proof_tbl->reject();
      }
    }
  }

  if (trans_tbl && child.proof_number == 0 && child.disproof_number == 0) {
    // If values weren't found in the proof table.
    
    boost::optional<transposition_table::entry_t> values = trans_tbl->query(hashes.top());

    if (values) {
      vertex::number_t const pn = values->first;
      vertex::number_t const dn = values->second;
      assert(pn != 0 && dn != 0);

      child.proof_number = pn;
      child.disproof_number = dn;
    }
  }

  if (child.proof_number == 0 && child.disproof_number == 0) {
    // If values were not found in any table.
    
    boost::optional<piece::color_t> winner;
    if (child.type != parent.type)  // Only check for win if this is the start of a move.
      winner = apns::winner(state.top(), player);

    if (winner) {
      if (*winner == attacker) {
        child.proof_number     = 0;
        child.disproof_number  = vertex::infty;
      } else {
        child.proof_number     = vertex::infty;
        child.disproof_number  = 0;
      }
    } else {
      // Check for repetitions.
      if (type == parent.type || !repetition(hashes.opponent_top(), history.records())) {
        child.proof_number    = 1;
        child.disproof_number = 1;
      } else {
        child.proof_number    = player == attacker ? vertex::infty : 0;
        child.disproof_number = player == attacker ? 0 : vertex::infty;
      }

    }

  }

  hashes.pop();
  state.pop(step);
}

bool simulate(vertex& parent, killer_db& killers, std::size_t ply, piece::color_t attacker,
              proof_table* proof_tbl, board_stack& boards, hashes_stack& hashes, history_stack& history) {
  assert(parent.children_count() == 0);

  piece::color_t const player = parent.type == vertex::type_or ? attacker : opponent_color(attacker);

  for (killer_db::ply_iterator killer = killers.ply_begin(ply + 1, parent.type);
      killer != killers.ply_end(ply + 1, parent.type);
      ++killer) {
    if (killer->revalidate(boards.top(), player)) {

      vertex::e_type type;
      if (parent.steps_remaining - static_cast<signed>(killer->steps_used()) >= 1)
        type = parent.type;
      else if (parent.steps_remaining - static_cast<signed>(killer->steps_used()) >= 0)
        type = opposite_type(parent.type);
      else
        continue;

      vertex::children_iterator child = parent.add_child();
      process_new(*child, parent, attacker, *killer, type, 0, proof_tbl, hashes, boards, history);

      if ((parent.type == vertex::type_or && child->proof_number == 0)
          || (parent.type == vertex::type_and && child->disproof_number == 0)) {
        update_numbers(parent);
        return true;
      }

      else if (parent.type == child->type) {
        boards.push(*killer);
        hashes.push(*child);
        history.push(*child, hashes.top());

        bool const success = simulate(*child, killers, ply + 1, attacker, proof_tbl, boards, hashes, history);

        history.pop(*child);
        hashes.pop();
        boards.pop(*killer);

        if (success) {
          update_numbers(parent);
          assert(parent.proof_number == 0 || parent.disproof_number == 0);
          return true;
        }
      }

      parent.resize(0);
    }
  }

  return false;
}

} // namespace apns::detail

search_stack::search_stack(zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash,
                           vertex* root, piece::color_t attacker, board const& initial_state) :
  path_(1, root),
  hashes_(1, initial_hash),
  history_(1, initial_hash),
  state_(initial_state),
  hasher_(&hasher),
  attacker_(attacker)
{ }

void search_stack::push(vertex* v) {
  assert(v);
  assert(!path_.empty());
  assert(!hashes_.empty());
  assert(!history_.empty());
  assert(v->step);
  assert(hasher_);

  vertex* const                 parent        = path_.back();
  zobrist_hasher::hash_t const  parent_hash   = hashes_.back();
  piece::color_t const          parent_player = vertex_player(*parent, attacker_);
  piece::color_t const          v_player      = vertex_player(*v, attacker_);
  zobrist_hasher::hash_t const  v_hash        = hasher_->update(
    parent_hash,
    v->step->step_sequence_begin(), v->step->step_sequence_end(),
    parent_player, v_player
  );

  path_.push_back(v);
  hashes_.push_back(v_hash);
  apply(*v->step, state_);

  if (v->type != parent->type)
    history_.push_back(v_hash);
}

void search_stack::pop() {
  assert(!at_root());
  assert(path_.size() >= 2);
  assert(hashes_.size() >= 2);

  vertex* const top = path_.back();
  vertex* const parent = *(path_.end() - 2);

  assert(top->step);

  path_.pop_back();
  hashes_.pop_back();
  unapply(*top->step, state_);

  if (top->type != parent->type)
    history_.pop_back();
}

void search_stack_checkpoint::revert() {
  if (stack_->path().size() < original_length_)
    throw std::logic_error("revert: The watched stack was modified below the checkpoint");

  while (stack_->path().size() > original_length_)
    stack_->pop();
}

void proof_number_search::do_iterate() {
  assert(game_);
  assert(!game_->root.step);

  search_stack stack(hasher_, initial_hash_, &game_->root, game_->attacker, game_->initial_state);

  while (!stack.path_top()->leaf()) {
    stack.push(best_successor(*stack.path_top()));
  }

  //process_leaf(path.begin(), path.end(), boards, hashes, history);

  garbage_collect();
}

void depth_first_pns::do_iterate() {
  using detail::cut;

  assert(game_);
  assert(!game_->root.step);

  assert(path_.empty() || path_.back()->children_count() == 0);
  assert(path_.empty() || path_.back()->proof_number <= limits_.back().pn_limit);
  assert(path_.empty() || path_.back()->disproof_number <= limits_.back().dn_limit);

  assert(hashes_.hashes().size() == path_.size());
  assert(limits_.size() == path_.size());

  process_leaf(path_.begin(), path_.end(), boards_, hashes_, history_);

  // Go up.
  while (!path_.empty() 
         && (path_.back()->proof_number == 0 || path_.back()->proof_number > limits_.back().pn_limit 
             || path_.back()->disproof_number > limits_.back().dn_limit)) {
    if (gc_high_ == 0)
      position_count_ -= cut(*path_.back());

    history_.pop(*path_.back());
    hashes_.pop();
    if (path_.back()->step)
      boards_.pop(*path_.back()->step);

    path_.pop_back();
    limits_.pop_back();
  }

  assert(path_.empty() || (path_.back()->proof_number > 0 && path_.back()->disproof_number > 0));

  // And go back down.
  while (!path_.empty() && path_.back()->children_count() > 0) {
    std::pair<vertex*, vertex*> best_two = two_best_successors(*path_.back());
    assert(best_two.first->proof_number > 0 && best_two.first->disproof_number > 0);

    limits_t new_limits = make_limits(
      *best_two.first,
      *path_.back(),
      best_two.second ? boost::optional<vertex&>(*best_two.second) : boost::none,
      limits_.back()
    );

    limits_.push_back(new_limits);
    path_.push_back(best_two.first);

    assert(path_.back()->proof_number <= limits_.back().pn_limit && path_.back()->disproof_number <= limits_.back().dn_limit);

    if (path_.back()->step)
      boards_.push(*path_.back()->step);
    hashes_.push(*path_.back());
    history_.push(*path_.back(), hashes_.top());
  }

  garbage_collect();
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

