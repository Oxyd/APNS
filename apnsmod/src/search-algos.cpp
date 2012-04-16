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
bool repetition(apns::search_stack const& stack) {
  using namespace apns;

  if (stack.history().size() >= 2) {
    if (stack.hasher().opponent_hash(stack.history().back()) == *(stack.history().end() - 2))
      return true;  // Loss because the last player's move has not resulted in a net change in game position.

    // Check for third-time repetitions. Only check against the last element in .history() as the previous ones should have
    // been checked by previous calls to this function.

    unsigned rep_count = 0;
    for (search_stack::history_sequence::const_iterator h = stack.history().begin();
         h != stack.history().end() - 1; ++h) {
      if (*h == stack.history().back())
        if (++rep_count == 3)
          return true;
    }
  }

  return false;
}

apns::piece::color_t vertex_player(apns::vertex const& v, apns::piece::color_t attacker) {
  return v.type == apns::vertex::type_or ? attacker : apns::opponent_color(attacker);
}

bool histories_compatible(apns::search_stack const& stack, apns::history_t const& history) {
  using namespace apns;

  boost::unordered_map<zobrist_hasher::hash_t, std::size_t> repetitions;
  bool repetition = false;

  for (history_t::const_iterator h = history.begin(); h != history.end(); ++h)
    ++repetitions[*h];

  for (search_stack::history_sequence::const_iterator h = stack.history().begin();
       h != stack.history().end() && !repetition;
       ++h) {
    if (++repetitions[*h] >= 3)
      repetition = true;
  }

  return !repetition;
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
  vertex::children_iterator best = std::min_element(parent.children_begin(), parent.children_end(),
                                                    vertex_comparator(parent));
  if (best != parent.children_end())
    return &*best;
  else
    return 0;
}

vertex const* best_successor(vertex const& parent) {
  vertex::const_children_iterator best = std::min_element(parent.children_begin(), parent.children_end(),
                                                          vertex_comparator(parent));
  if (best != parent.children_end())
    return &*best;
  else
    return 0;
}

std::pair<vertex const*, vertex const*> two_best_successors(vertex const& parent) {
  if (parent.children_count() >= 2) {
    vertex::const_children_iterator best = parent.children_begin();
    vertex::const_children_iterator second_best = boost::next(parent.children_begin());
    vertex_comparator better(parent);

    if (better(*second_best, *best))
      std::swap(best, second_best);

    for (vertex::const_children_iterator child = boost::next(second_best); child != parent.children_end(); ++child) {
      if (better(*child, *best)) {
        second_best = best;
        best = child;
      } else if (better(*child, *second_best) && child != best)
        second_best = child;
    }

    assert(best != second_best);

    return std::make_pair(&*best, &*second_best);
  } else {
    return std::make_pair(best_successor(parent), static_cast<vertex const*>(0));
  }
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
  if (at_root())
    throw std::logic_error("pop: Attempted to pop root");

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


void search_tree::expand() {
  vertex* const leaf = stack_.path_top();
  if (!leaf->leaf())
    throw std::logic_error("expand: Attempt to expand a non-leaf vertex");

  // Try to simulate the leaf first.
  if (killers_ && simulate())
    return;

  // Make a list of all possible steps.
  typedef std::vector<std::pair<step, vertex::e_type> > steps_seq;
  steps_seq steps;

  piece::color_t const player = vertex_player(*leaf, attacker_);
  for (all_steps_iter new_step = all_steps_begin(stack_.state(), player); new_step != all_steps_end(); ++new_step) {
    int const remaining = leaf->steps_remaining - static_cast<signed>(new_step->steps_used());

    if (remaining >= 0) {
      steps.push_back(std::make_pair(*new_step, opposite_type(leaf->type)));
      if (remaining >= 1)
        steps.push_back(std::make_pair(*new_step, leaf->type));
    }
  }

  leaf->resize(steps.size());

  vertex::children_iterator killers_end = leaf->children_begin();
  vertex::children_iterator child       = leaf->children_begin();

  for (steps_seq::const_iterator s = steps.begin(); s != steps.end(); ++s, ++child) {
    step const            step = s->first;
    vertex::e_type const  type = s->second;

    make_child(*child, step, type);

    if (killers_ && killers_->is_killer(stack_.size(), type, step) && child != killers_end)
      leaf->swap_children(child, killers_end++);
  }

  size_ += leaf->children_count();
}

void search_tree::cut_children() {
  vertex_counter cut_counter;
  vertex& parent = *stack_.path_top();

  if (!parent.leaf() &&
      ((parent.type == vertex::type_or && parent.proof_number == 0)
       || (parent.type == vertex::type_and && parent.disproof_number == 0))) {
    vertex::number_t vertex::* num = parent.type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number;

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
  }
  else if (parent.proof_number == 0 || parent.disproof_number == 0)
    // All children constitute a proof here.
    { }
  else {
    // No proof.
    traverse(parent, backtrack(), boost::ref(cut_counter));
    parent.resize(0);
    parent.pack();

    --cut_counter.count;  // parent has not been removed, but it's been counted.
  }

  size_ -= cut_counter.count;
}

void search_tree::update_path() {
  search_stack stack(stack_);  // Make a copy because we're going to ruin the stack.

  while (true) {
    std::size_t const ply = stack.size();
    vertex& current = *stack.path_top();

    update_numbers(current);

    if (killers_ && stack.size() >= 2) {
      vertex const& parent = **(stack.path().end() - 2);
      if ((parent.type == vertex::type_or && current.proof_number == 0) ||
          (parent.type == vertex::type_and && current.disproof_number == 0)) {
        killers_->add(ply, parent.type, *current.step);
      }
    }

    if (proof_tbl_ && (current.proof_number == 0 || current.disproof_number == 0)) {
      proof_tbl_->insert(stack.hashes_top(), ply,
                         proof_entry_t(current.proof_number, current.disproof_number, stack.history()));
    }

    if (trans_tbl_ && (current.proof_number != 0 && current.disproof_number != 0)) {
      trans_tbl_->insert(stack.hashes_top(), ply, std::make_pair(current.proof_number, current.disproof_number));
    }

    if (!stack.at_root())
      stack.pop();
    else
      break;
  }
}

bool search_tree::simulate() {
  assert(stack_.path_top()->leaf());
  assert(killers_);

  search_stack_checkpoint checkpoint(stack_);

  vertex* const         parent  = stack_.path_top();
  std::size_t const     ply     = stack_.size();
  piece::color_t const  player  = vertex_player(*parent, attacker_);

  for (killer_db::ply_iterator killer = killers_->ply_begin(ply + 1, parent->type);
       killer != killers_->ply_end(ply + 1, parent->type); ++killer) {
    if (killer->revalidate(stack_.state(), player)) {
      int const remaining = parent->steps_remaining - static_cast<signed>(killer->steps_used());

      vertex::e_type type;
      if (remaining >= 1)
        type = parent->type;
      else if (remaining == 0)
        type = opposite_type(parent->type);
      else
        continue;

      vertex::children_iterator child = parent->add_child();
      make_child(*child, *killer, type);

      if ((parent->type == vertex::type_or && child->proof_number == 0) ||
          (parent->type == vertex::type_and && child->disproof_number == 0)) {
        update_numbers(*parent);
        return true;
      }
      else if (parent->type == child->type) {
        stack_.push(&*child);
        if (simulate()) {
          update_numbers(*parent);
          return true;
        }
      }
    }
  }

  return false;
}

void search_tree::make_child(vertex& child, step const& step, vertex::e_type type) {
  search_stack_checkpoint checkpoint(stack_);

  child.step = step;
  child.type = type;
  child.proof_number = 0;
  child.disproof_number = 0;

  vertex const* const parent = stack_.path_top();
  stack_.push(&child);

  if (child.type == parent->type)
    child.steps_remaining = parent->steps_remaining - static_cast<signed>(step.steps_used());
  else
    child.steps_remaining = MAX_STEPS;

  if (proof_tbl_) {
    boost::optional<proof_table::entry_t> values = proof_tbl_->query(stack_.hashes_top());

    if (values && histories_compatible(stack_, values->history)) {
      child.proof_number = values->proof_number;
      child.disproof_number = values->disproof_number;
    } else if (values) {
      proof_tbl_->reject();
    }
  }

  if (trans_tbl_ && child.proof_number == 0 && child.disproof_number == 0) {
    // Values weren't found in the proof table.
    boost::optional<transposition_table::entry_t> values = trans_tbl_->query(stack_.hashes_top());

    if (values) {
      child.proof_number = values->first;
      child.disproof_number = values->second;
    }
  }

  if (child.proof_number == 0 && child.disproof_number == 0) {
    // Values weren't found in either of the tables.
    piece::color_t const player = vertex_player(*parent, attacker_);
    boost::optional<piece::color_t> winner;
    if (child.type != parent->type)  // Only check for win if this is the start of a move.
      winner = apns::winner(stack_.state(), player);

    if (winner) {
      if (*winner == attacker_) {
        child.proof_number = 0;
        child.disproof_number = vertex::infty;
      } else {
        child.proof_number = vertex::infty;
        child.disproof_number = 0;
      }
    } else {
      // Last chance is losing due to a repetition.
      if (type != parent->type && repetition(stack_)) {
        child.proof_number = player == attacker_ ? vertex::infty : 0;
        child.disproof_number = player == attacker_ ? 0 : vertex::infty;
      } else {
        child.proof_number = 1;
        child.disproof_number = 1;
      }
    }
  }
}

void proof_number_search::do_iterate() {
  assert(game_);
  assert(!game_->root.step);

  tree_.select_root();

  while (!tree_.current().leaf()) {
    select_best(tree_);
  }

  tree_.expand();
  tree_.update_path();
}

void depth_first_pns::do_iterate() {
  assert(game_);
  assert(!game_->root.step);
  assert(limits_.size() == tree_.selection_depth());
  assert(!finished());

  // Go up while we're at a vertex whose numbers are off-limits.
  while (!tree_.at_root() &&
         (tree_.current().proof_number == 0 || tree_.current().proof_number > limits_.back().pn_limit ||
          tree_.current().disproof_number == 0 || tree_.current().disproof_number > limits_.back().dn_limit)) {
    tree_.cut_children();
    tree_.select_parent();
    limits_.pop_back();

    assert(!limits_.empty());
  }

  assert(tree_.current().proof_number > 0 && tree_.current().disproof_number > 0);

  // And go back down until we reach a leaf.
  while (!tree_.current().leaf()) {
    std::pair<vertex const*, vertex const*> best_two = two_best_successors(tree_.current());
    assert(best_two.first);
    assert(best_two.first->proof_number > 0 && best_two.first->disproof_number > 0);

    limits_.push_back(make_limits(
      *best_two.first,
      tree_.current(),
      best_two.second ? boost::optional<vertex const&>(*best_two.second) : boost::none,
      limits_.back()
    ));
    tree_.select_child(best_two.first);

    assert(tree_.current().proof_number <= limits_.back().pn_limit);
    assert(tree_.current().disproof_number <= limits_.back().dn_limit);
  }

  tree_.expand();
  tree_.update_path();
}

depth_first_pns::limits_t depth_first_pns::make_limits(vertex const& v, vertex const& parent,
                                                       boost::optional<vertex const&> second_best,
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

