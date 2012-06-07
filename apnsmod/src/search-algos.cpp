#include "search-algos.hpp"
#include "board.hpp"
#include "movement.hpp"
#include "tree.hpp"
#include "util.hpp"

#include <boost/integer.hpp>
#include <boost/bind.hpp>

#include <cassert>
#include <vector>
#include <set>
#include <iostream>
#include <algorithm>

namespace {

bool histories_compatible(apns::search_stack const& stack,
                          apns::history_t const& history) {
  using namespace apns;

  boost::unordered_map<zobrist_hasher::hash_t, std::size_t> repetitions;
  bool repetition = false;

  for (history_t::const_iterator h = history.begin(); h != history.end(); ++h)
    ++repetitions[*h];

  for (search_stack::history_sequence::const_iterator h =
         stack.history().begin();
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
boost::optional<piece::color_t> 
winner(board const& board, piece::color_t player) {
  piece::color_t const opponent = opponent_color(player);

  position::row_t const players_target_row =
    player == piece::gold ? position::MAX_ROW : position::MIN_ROW;
  position::row_t const opponents_target_row =
    player == piece::gold ? position::MIN_ROW : position::MAX_ROW;

  bool player_has_rabbits = false;
  bool opponent_has_rabbits = false;

  for (position::col_t col = position::MIN_COLUMN; col <= position::MAX_COLUMN;
       ++col) {
    boost::optional<piece> const piece =
      board.get(position(players_target_row, col));
    if (piece && piece->type() == piece::rabbit) {
      if (piece->color() == player) {
        // Player has reached goal.
        return player;
      } else {
        opponent_has_rabbits = true;
      }
    }
  }

  for (position::col_t col = position::MIN_COLUMN; col <= position::MAX_COLUMN;
       ++col) {
    boost::optional<piece> const piece =
      board.get(position(opponents_target_row, col));
    if (piece && piece->type() == piece::rabbit) {
      if (piece->color() == opponent) {
        // Opponent has reached goal.
        return opponent;
      } else {
        player_has_rabbits = true;
      }
    }
  }

  // No goal.

  if (!(player_has_rabbits && opponent_has_rabbits)) {
    // It is possible for one player to lose due to loss of all rabbits.

    for (
      position::row_t row = position::MIN_ROW + 1;
      row <= position::MAX_ROW - 1;
      ++row
    ) {  // We've already cheched first and last row.
      for (
        position::col_t col = position::MIN_COLUMN;
        col <= position::MAX_COLUMN;
        ++col
      ) {
        boost::optional<piece> const piece = board.get(position(row, col));
        if (piece && piece->type() == piece::rabbit) {
          if (piece->color() == player)
            player_has_rabbits = true;
          else
            opponent_has_rabbits = true;
        }
      }
    }

    if (!opponent_has_rabbits)
      return player;
    if (!player_has_rabbits)
      return opponent;
  }

  return boost::none;
}

vertex* best_successor(vertex& parent) {
  return const_cast<vertex*>(best_successor(
    const_cast<vertex const&>(parent)
  ));
}

vertex const* best_successor(vertex const& parent) {
  if (parent.size() > 0) {
    vertex::const_iterator best = parent.begin();
    vertex_comparator better(parent);

    for (vertex::const_iterator child = boost::next(best);
         child != parent.end(); ++child) {
      if (better(*child, *best)) {
        best = child;
      }
    }

    return &*best;
  } else {
    return 0;
  }
}

std::pair<vertex*, vertex*>
two_best_successors(vertex& parent) {
  if (parent.size() >= 2) {
    vertex::iterator best = parent.begin();
    vertex::iterator second_best =
      boost::next(parent.begin());
    vertex_comparator better(parent);

    if (better(*second_best, *best))
      std::swap(best, second_best);

    for (vertex::iterator child = boost::next(second_best);
         child != parent.end(); ++child) {
      if (better(*child, *best)) {
        second_best = best;
        best = child;
      } else if (better(*child, *second_best) && child != best)
        second_best = child;
    }

    assert(best != second_best);

    assert(&*best == best_successor(parent));
    return std::make_pair(&*best, &*second_best);
  } else {
    return std::make_pair(best_successor(parent),
                          static_cast<vertex*>(0));
  }
}

void killer_db::add(std::size_t ply, step const& step) {
  if (ply >= killers_.size())
    killers_.resize(ply + 1, ply_killers_t(killer_count_));

  if (std::find(killers_[ply].begin(), killers_[ply].end(), step) ==
      killers_[ply].end()) {
    if (!killers_[ply].full())
      ++size_;  // Otherwise this replaces an existing record.

    killers_[ply].push_back(step);
  }
}

bool killer_db::is_killer(
  std::size_t ply, step const& step
) const {
  if (killers_.size() > ply)
    return
      std::find(killers_[ply].begin(), killers_[ply].end(), step) !=
      killers_[ply].end();
  else return false;
}

void history_table::insert(step const& step, std::size_t depth) {
  table_t::mapped_type const value = depth * depth;
  table_[step] += value;
}

void history_table::sort(vertex& v) const {
  stable_sort_children(v, compare(table_));

#if 0
  vertex::iterator sorted_end = v.begin();

  while (sorted_end != v.end()) {
    vertex::iterator max = v.end();
    table_t::mapped_type max_value = 0;

    for (vertex::iterator child = sorted_end; 
         child != v.end(); ++child) {
      if (child->step) {
        history_table::table_t::const_iterator value =
          table_.find(*child->step);
        if (value != table_.end() &&
            (value->second > max_value || max == v.end())) {
          max = child;
          max_value = value->second;
        }
      }
    }

    if (max != v.end()) {
      if (max != sorted_end) {
        v.swap_children(sorted_end, max);
      }

      ++sorted_end;
    } else {
      // No child's history value is greater than the current one's. We can
      // terminate the sort here.

      break;
    }
  }
#endif
}

bool history_table::compare::operator () (
  vertex const& lhs, vertex const& rhs
) {
  // Note: Lexicographical sorting with PN/DN values as the primary key would
  // require that resort_children doesn't destroy the secondary sort order,
  // which it currently does.

  history_table::table_t::const_iterator l = table_->find(*lhs.step);
  history_table::table_t::const_iterator r = table_->find(*rhs.step);

  boost::uint64_t const left  = l != table_->end() ? l->second : 0;
  boost::uint64_t const right = r != table_->end() ? r->second : 0;

  return left > right;
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

  // A number type that can hold values as large as 2 * vertex::max. This will
  // be used to detect overflows.
  typedef boost::uint_value_t<2 * vertex::max_num>::fast sum_num_t;

  vertex::number_t  min = vertex::infty;
  sum_num_t         sum = 0;

  for (vertex::iterator child = v.begin();
       child != v.end(); ++child) {
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

search_stack::search_stack(
  zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash,
  vertex* root, piece::color_t attacker, board const& initial_state
) : path_(1, root)
  , hashes_(1, initial_hash)
  , history_(1, initial_hash)
  , state_(initial_state)
  , state_pos_(0)
  , hasher_(&hasher)
  , attacker_(attacker)
{ }

void search_stack::push(vertex* v) {
  assert(v);
  assert(!path_.empty());
  assert(!hashes_.empty());
  assert(!history_.empty());
  assert(hasher_);

  vertex* const                 parent        = path_.back();
  zobrist_hasher::hash_t const  parent_hash   = hashes_.back();
  piece::color_t const          parent_player =
    vertex_player(*parent, attacker_);
  piece::color_t const          v_player      = vertex_player(*v, attacker_);
  zobrist_hasher::hash_t const  v_hash        =
    v->step
      ? hasher_->update(
          parent_hash,
          v->step->begin(), v->step->end(),
          parent_player, v_player, parent->steps_remaining
        )
      : parent_hash;

  int stage = 0;

  try {
    path_.push_back(v); ++stage;
    hashes_.push_back(v_hash); ++stage;
    if (v->type != parent->type) { history_.push_back(v_hash); ++stage; }
  } catch (...) {
    switch (stage) {
    case 3:   history_.pop_back();
    case 2:   hashes_.pop_back();
    case 1:   path_.pop_back();
    }

    throw;
  }
}

void search_stack::pop() {
  if (at_root())
    throw std::logic_error("pop: Attempted to pop root");

  assert(path_.size() >= 2);

  vertex* const top = path_.back();
  vertex* const parent = *(path_.end() - 2);

  if (state_pos_ == path_.size() - 1) {
    if (top->step)
      unapply(*top->step, state_);
    --state_pos_;
  }

  path_.pop_back();
  hashes_.pop_back();
  if (top->type != parent->type) history_.pop_back();
}

void search_stack::reset_to_root() {
  while (!at_root())
    pop();
}

board const& search_stack::state() const {
  assert(state_pos_ <= path_.size() - 1);

  while (state_pos_ != path_.size() - 1) {
    ++state_pos_;
    if (path_[state_pos_]->step)
      apply(*path_[state_pos_]->step, state_);
  }

  assert(state_pos_ == path_.size() - 1);
  return state_;
}

std::ostream& operator << (std::ostream& out, search_stack const& stack) {
  format_path(out, stack.path().begin(), stack.path().end());
  return out;
}

void search_stack_checkpoint::revert() {
  while (stack_->path_top() != original_top_) {
    if (!stack_->at_root())
      stack_->pop();
    else
      throw std::logic_error(
        "revert: The watched stack was modified below the checkpoint"
      );
  }
}

std::ostream& format_path(
  std::ostream& out,
  apns::search_stack::path_sequence::const_iterator begin,
  apns::search_stack::path_sequence::const_iterator end
) {
  apns::search_stack::path_sequence::const_iterator v = begin;
  while (v != end) {
    out << (v != begin ? " -> " : "")
        << ((**v).step ?
            (**v).step->to_string() :
            (v == begin ? "root" : "lambda"))
        << " (" << ((**v).type == apns::vertex::type_or ? "OR" : "AND") << ")";

    ++v;
  }

  return out;
}

bool try_apply(move_path const& path, board& board) {
  for (move_path::const_iterator v = path.begin(); v != path.end(); ++v) {
    if (*v && (**v).step) {
      if (!try_apply(*(**v).step, board)) {
        // Revert the partial application.

        if (v != path.begin()) {
          do {
            --v;
            if (*v && (**v).step) unapply(*(**v).step, board);
          } while (v != path.begin());
        }

        return false;
      }
    }
  }

  return true;
}

void unapply(move_path const& path, board& board) {
  for (move_path::const_reverse_iterator v = path.rbegin(); v != path.rend();
       ++v) {
    if (*v && (**v).step) {
      unapply(*(**v).step, board);
    }
  }
}

move_history_seq move_history(search_stack const& stack) {
  search_stack::path_sequence::const_reverse_iterator v = stack.path().rbegin();
  search_stack::hashes_sequence::const_iterator hash = stack.hashes().end();
  move_history_seq result = {{}};

  vertex::e_type const type = stack.path_top()->type;
  while (v != stack.path().rend() && (**v++).type == type) --hash;
  std::copy(hash, stack.hashes().end(), result.begin());

  return result;
}

vertex* parent(search_stack const& stack) {
  if (!stack.at_root())
    return *boost::next(stack.path().rbegin());
  else
    return 0;
}

std::size_t ply(search_stack const& stack) {
  return stack.history().size();
}

void history_order(history_table const& history, vertex& v) {
  history.sort(v);
}

void killer_order(killer_db const& killers, std::size_t level, vertex& v) {
  vertex::iterator killers_end = v.begin();

  for (
    vertex::iterator child = v.begin();
    child != v.end();
    ++child
  ) {
    if (child != killers_end && child->step &&
        killers.is_killer(level, *child->step))
      v.swap_children(child, killers_end++);
  }
}

std::size_t expand(vertex& leaf, board const& state, piece::color_t attacker,
                   move_history_seq const& move_hist,
                   zobrist_hasher const& hasher) {
  if (!leaf.leaf())
    throw std::logic_error("expand: Attempt to expand a non-leaf vertex");

  typedef std::vector<std::pair<step, vertex::e_type> > steps_seq;

  steps_seq steps;
  piece::color_t player = vertex_player(leaf, attacker);

  for (
    all_steps_iter new_step = all_steps_begin(state, player);
    new_step != all_steps_end();
    ++new_step
  ) {
    int const remaining =
      leaf.steps_remaining - static_cast<signed>(new_step->steps_used());

    if (remaining >= 1) {
      zobrist_hasher::hash_t child_hash = hasher.update(
        last(move_hist),
        new_step->begin(), new_step->end(),
        player, player, leaf.steps_remaining
      );
      if (std::find(move_hist.begin(), move_hist.end(), child_hash) ==
          move_hist.end())
        steps.push_back(std::make_pair(*new_step, leaf.type));
    } else if (remaining == 0)
      steps.push_back(std::make_pair(*new_step, opposite_type(leaf.type)));
  }

  bool const make_lambda = leaf.steps_remaining > 1;

  leaf.resize(steps.size() + (make_lambda ? 1 : 0));
  vertex::iterator child = leaf.begin();

  for (
    steps_seq::const_iterator step = steps.begin();
    step != steps.end();
    ++step, ++child
  ) {
    child->proof_number = child->disproof_number = 1;
    child->step = step->first;
    child->type = step->second;
    if (child->type == leaf.type)
      child->steps_remaining = leaf.steps_remaining - child->step->steps_used();
    else
      child->steps_remaining = MAX_STEPS;
  }

  if (make_lambda) {
    assert(child != leaf.end());
    child->proof_number = child->disproof_number = 1;
    child->type = leaf.type;
    child->steps_remaining = leaf.steps_remaining - 1;
  }

  return leaf.size();
}

void evaluate(search_stack& stack, piece::color_t attacker, log_sink& log) {
  vertex& child = *stack.path_top();
  vertex const* const parent = *boost::next(stack.path().rbegin());

  piece::color_t const player = vertex_player(*parent, attacker);

  boost::optional<piece::color_t> winner;

  // Only check for win if a rabbit was moved or captured in this move.

  search_stack::path_sequence::const_reverse_iterator iter =
    stack.path().rbegin();

  do {  // Iterate over the move.
    vertex* const current = *iter;
    if (current->step && current->step->moves_rabbit()) {
      winner = apns::winner(stack.state(), player);
      break;
    }

    ++iter;
  } while (iter != stack.path().rend() && (**iter).type == parent->type);

  if (winner) {
    // If this isn't the start of a move, only consider wins, not losses.

    if (child.type != parent->type || *winner == player) {
      log << stack << " proved by evaluation function\n";

      if (*winner == attacker) {
        child.proof_number = 0;
        child.disproof_number = vertex::infty;
      } else if (child.type != parent->type) {
        child.proof_number = vertex::infty;
        child.disproof_number = 0;
      }
    }
  } else if (child.type != parent->type) {
    // Last chance is losing due to a repetition.
    if (repetition(stack)) {
      log << stack << " proved by repetition\n";

      child.proof_number = player == attacker ? vertex::infty : 0;
      child.disproof_number = player == attacker ? 0 : vertex::infty;
    }
  }
}

bool pt_lookup(proof_table& pt, search_stack& stack) {
  vertex& child = *stack.path_top();

  boost::optional<proof_table::entry_t> values = pt.query(stack.hashes_top());

  if (values && histories_compatible(stack, values->history)) {
    child.proof_number = values->proof_number;
    child.disproof_number = values->disproof_number;

    return true;
  } else if (values) {
    pt.reject();
  }

  return false;
}

void pt_store(proof_table& pt, vertex const& v,
              zobrist_hasher::hash_t hash,
              std::size_t ply,
              search_stack::history_sequence::const_iterator history_begin,
              search_stack::history_sequence::const_iterator history_end) {
  proof_entry_t entry(
    v.proof_number, v.disproof_number,
    history_t(std::distance(history_begin, history_end))
  );

  std::copy(history_begin, history_end,
            entry.history.begin());

  pt.insert(hash, ply, entry);
}

bool tt_lookup(transposition_table& tt, zobrist_hasher::hash_t hash,
               vertex& child) {
  boost::optional<transposition_table::entry_t> values = tt.query(hash);

  if (values) {
    child.proof_number = values->proof_number;
    child.disproof_number = values->disproof_number;

    return true;
  }

  return false;
}

void tt_store(transposition_table& tt, vertex const& v,
              zobrist_hasher::hash_t hash, std::size_t ply) {
  tt.insert(hash, ply,
            transposition_entry(v.proof_number, v.disproof_number));
}

std::size_t garbage_collect(std::size_t how_many, search_stack stack) {
  // NB: This function destroys the passed-in stack, so it's accepted by copy.

  vertex* avoid = 0;
  if (!stack.at_root())
    avoid = *boost::next(stack.path().begin());

  stack.reset_to_root();

  if (stack.path_top()->proof_number == 0 ||
      stack.path_top()->disproof_number == 0)
    return 0;

  std::size_t removed = 0;

  while (removed < how_many) {
    removed += collect_proved(*stack.path_top());
    
    vertex& current = *stack.path_top();
    if (!current.leaf()) {
      vertex_comparator worse(current, false);
      vertex::iterator worst = current.end();

      for (vertex::iterator child = current.begin();
           child != current.end(); ++child) {
        if (!child->leaf() && &*child != avoid &&
            (worst == current.end() || worse(*child, *worst)))
          worst = child;
      }

      if (worst != current.end()) {
        stack.push(&*worst);
      } else if (!stack.at_root()) {
        assert(stack.path_top() != avoid);

        removed += cut(*stack.path_top());
        stack.pop();
      } else {
        break;
      }

    } else {
      if (!stack.at_root()) {
        stack.pop();
      } else {
        break;
      }
    }
  }

  return removed;
}

std::size_t collect_proved(vertex& parent) {
  std::size_t removed = 0;

  for (vertex::iterator child = parent.begin();
       child != parent.end(); ++child) {
    if (child->type != parent.type &&
        ((parent.type == vertex::type_or && child->disproof_number == 0) ||
         (parent.type == vertex::type_and && child->proof_number == 0))) {
      removed += cut(*child);
    }
  }
  
  return removed;
}

std::size_t cut(vertex& parent) {
  vertex_counter cut_counter;
  traverse(parent, backtrack(), boost::ref(cut_counter));

  parent.resize(0);
  parent.pack();

  return cut_counter.count - 1;  // parent has been counted but not removed.
}

namespace {

void undo_add_child(vertex& parent, std::size_t& size) {
  assert(parent.size() == 1);
  parent.remove(parent.begin());
  --size;
}

}

bool simulate(search_stack& stack, piece::color_t attacker,
              std::size_t& size,
              killer_db const& killers,
              boost::shared_ptr<proof_table> const& proof_tbl,
              log_sink& log) {
  std::size_t const level     = stack.size();
  vertex& parent              = *stack.path_top();
  board const& position       = stack.state();
  piece::color_t const player = vertex_player(parent, attacker);

  for (
    killer_db::level_iterator killer = killers.level_begin(level);
    killer != killers.level_end(level);
    ++killer
  ) {
    step_holder step = revalidate(*killer, position, player);
    if (step && parent.steps_remaining - step->steps_used() >= 0) {
      assert(parent.leaf());

      // First append the step as a new child of opposite type (so that it makes
      // sense to evaluate it at all) and see if that causes a proof. If it
      // doesn't, and it's got enough steps remaining, flip its type and try
      // one level deeper.
      //
      // This uses built-in recursion. It should be okay, since the recursion
      // is at most four calls deep.

      vertex::iterator child = parent.add();
      ++size;

      transaction child_added(boost::bind(
        &undo_add_child,
        boost::ref(parent),
        boost::ref(size)
      ));

      child->step = *step;
      child->proof_number = child->disproof_number = 1;
      child->steps_remaining = MAX_STEPS;
      child->type = opposite_type(parent.type);

      search_stack_checkpoint checkpoint(stack);
      stack.push(&*child);

      bool const found_in_pt = proof_tbl && pt_lookup(*proof_tbl, stack);
      if (!found_in_pt)
        evaluate(stack, attacker, log);

      if (cutoff(parent, *child)) {
        log << stack << " proved by simulation\n";

        update_numbers(parent);
        assert(parent.proof_number == 0 || parent.disproof_number == 0);

        child_added.commit();

        return true;
      }
    }
  }

  assert(parent.leaf());
  return false;
}

//! Check whether the game would be lost due to a repetition if the
//! given player made the given step from the given position assuming the
//! passed-in game history.
bool repetition(search_stack const& stack) {
  if (stack.history().size() >= 2) {
    // Check for null moves.
    if (stack.hasher().opponent_hash(stack.history().back()) ==
        *(stack.history().end() - 2))
      return true;

    // Check for third-time repetitions.
    // Only check against the last element in .history() as the previous ones
    // should have been checked by previous calls to this function.

    unsigned rep_count = 0;
    for (search_stack::history_sequence::const_iterator h =
         stack.history().begin(); h != stack.history().end() - 1; ++h) {
      if (*h == stack.history().back())
        if (++rep_count == 3)
          return true;
    }
  }

  return false;
}

piece::color_t vertex_player(vertex const& v, piece::color_t attacker) {
  return v.type == vertex::type_or ? attacker : opponent_color(attacker);
}

void proof_number_search::do_iterate() {
  assert(game_);
  assert(!game_->root.step);
  assert(log_);
  assert(stack_.at_root());

  while (!stack_.path_top()->leaf()) {
    if (history_tbl_)
      history_order(*history_tbl_, *stack_.path_top());

    if (killer_db_)
      killer_order(*killer_db_, stack_.size(), *stack_.path_top());

    push_best(stack_);
  }

  expand_and_eval();

  while (true) {
    vertex& current = *stack_.path_top();

    vertex* parent = 0;
    if (!stack_.at_root())
      parent = apns::parent(stack_);

    update_and_store(
      current, parent,
      stack_.path().begin(), stack_.path().end(),
      stack_.history().begin(), stack_.history().end(),
      stack_.hashes_top()
    );

    if (parent)
      stack_.pop();
    else
      break;
  }

  if (gc_enabled() && size_ > gc_high_ && gc_low_ <= size_)
    size_ -= garbage_collect(size_ - gc_low_, stack_);
}

void depth_first_pns::do_iterate() {
  assert(game_);
  assert(!game_->root.step);
  assert(limits_.size() == stack_.size());
  assert(!finished());
  assert(log_);

  // Go up while we're at a vertex whose numbers are off-limits.
  vertex* current = stack_.path_top();
  while (!stack_.at_root() &&
         (current->proof_number == 0 || 
          current->proof_number > limits_.back().pn_limit ||
          current->disproof_number == 0 || 
          current->disproof_number > limits_.back().dn_limit)) {
    bool const cut = !gc_enabled() && current->type != parent(stack_)->type;

    stack_.pop();
    limits_.pop_back();

    if (cut)
      size_ -= apns::cut(*current);

    current = stack_.path_top();

    assert(!limits_.empty());
  }

  assert(current->proof_number > 0 && current->disproof_number > 0);
  assert(current->proof_number <= limits_.back().pn_limit &&
         current->disproof_number <= limits_.back().dn_limit);

  // And go back down until we reach a leaf.
  while (!current->leaf()) {
    std::pair<vertex*, vertex*> best_two = two_best_successors(*current);

    assert(best_two.first);
    assert(best_two.first->proof_number > 0 && 
           best_two.first->disproof_number > 0);

    limits_.push_back(make_limits(
      *best_two.first,
      *current,
      best_two.second ? 
        boost::optional<vertex const&>(*best_two.second) : boost::none,
      limits_.back()
    ));

    stack_.push(best_two.first);
    current = best_two.first;

    assert(current->proof_number <= limits_.back().pn_limit);
    assert(current->disproof_number <= limits_.back().dn_limit);
  }

  size_ += expand(*current, stack_.state(), game_->attacker,
                  move_history(stack_), hasher_);
  evaluate_children();

  search_stack::path_sequence::const_reverse_iterator
    current_path = stack_.path().rbegin();
  search_stack::hashes_sequence::const_reverse_iterator
    current_hash = stack_.hashes().rbegin();
  search_stack::history_sequence::const_reverse_iterator
    current_history = stack_.history().rbegin();
  std::size_t ply = stack_.history().size();
  
  while (true) {
    vertex* parent = 0;
    if (boost::next(current_path) != stack_.path().rend())
      parent = *boost::next(current_path);

    update_and_store(
      *current, parent,
      stack_.path().begin(), current_path.base(),
      stack_.history().begin(), current_history.base(),
      *current_hash
    );

    if (parent) {
      if (parent->type != current->type) {
        ++current_history;
        --ply;
      }

      current = *++current_path;
      ++current_hash;
    } else {
      break;
    }
  }

  if (gc_enabled() && size_ > gc_high_ && gc_low_ <= size_)
    size_ -= garbage_collect(size_ - gc_low_, stack_);
}

depth_first_pns::limits_t 
depth_first_pns::make_limits(
  vertex const& v, vertex const& parent,
  boost::optional<vertex const&> second_best,
  limits_t parent_limits
) {
  double const EPSILON = 0.5;

  vertex::number_t vertex::* min_num = 
    v.type == vertex::type_or ? &vertex::proof_number
                              : &vertex::disproof_number;
  vertex::number_t limits_t::* min_lim =
    v.type == vertex::type_or ? &limits_t::pn_limit
                              : &limits_t::dn_limit;
  vertex::number_t vertex::* dif_num = 
    v.type == vertex::type_or ? &vertex::disproof_number
                              : &vertex::proof_number;
  vertex::number_t limits_t::* dif_lim =
    v.type == vertex::type_or ? &limits_t::dn_limit
                              : &limits_t::pn_limit;

  limits_t new_limits;

  vertex::number_t second_num = 
    second_best ? (*second_best).*min_num : v.*min_num;
  new_limits.*min_lim = std::min(
    parent_limits.*min_lim,
    //second_num + 1
    static_cast<vertex::number_t>(std::ceil(second_num * (1 + EPSILON)))
  );

  if (parent_limits.*dif_lim < vertex::infty)
    new_limits.*dif_lim = 
      parent_limits.*dif_lim - parent.*dif_num + v.*dif_num;
  else
    new_limits.*dif_lim = vertex::infty;

  assert(v.*min_num <= new_limits.*min_lim);
  assert(v.*dif_num <= new_limits.*dif_lim);

  return new_limits;
}

} // namespace apns

