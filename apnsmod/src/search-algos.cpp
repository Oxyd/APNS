#include "search-algos.hpp"
#include "board.hpp"
#include "movement.hpp"
#include "tree.hpp"
#include "util.hpp"

#include <boost/integer.hpp>
#include <boost/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/iterator/indirect_iterator.hpp>

#include <cassert>
#include <vector>
#include <set>
#include <iostream>
#include <algorithm>
#include <limits>

namespace {

bool histories_compatible(apns::search_stack const& stack,
                          apns::history_t const& history) {
  using namespace apns;

  if (stack.history().size() >= 2 && history.size() >= 2) {
    if (stack.hasher().opponent_hash(history.back()) == *(stack.history().end() - 2))
      return false;

    for (search_stack::history_sequence::const_iterator x = stack.history().begin();
         x != stack.history().end(); ++x) {
      std::size_t rep = 1;
      for (history_t::const_iterator y = history.begin(); y != history.end(); ++y)
        if (*y == *x && ++rep == 3) return false;

      if (rep == 2)
        for (search_stack::history_sequence::const_iterator y = boost::next(x); 
             y != stack.history().end(); ++y)
          if (*y == *x) return false;
    }
  } 

  return true;
}

} // anonymous namespace

namespace apns {

/**
 * Did any player win?
 *
 * \param board The game situation.
 * \param player Last player to have made a step.
 * \return If any player has won, return their color; otherwise return nothing.
 */
boost::optional<piece::color_t> 
winner(board const& board, piece::color_t player) {
  piece::color_t const opponent = opponent_color(player);

  // Official rules state the following:
  //
  //   The order of checking for win/lose conditions is as follows assuming
  //   player A just made the move and player B now needs to move:
  //
  //   1. Check if a rabbit of player A reached goal. If so player A wins.
  //   2. Check if a rabbit of player B reached goal. If so player B wins.
  //   3. Check if player B lost all rabbits. If so player A wins.
  //   4. Check if player A lost all rabbits. If so player B wins.

  board::mask const player_target =
    player == piece::gold
      ? board::mask::row(position::MAX_ROW)
      : board::mask::row(position::MIN_ROW);
  board::mask const opponent_target =
    player == piece::gold
      ? board::mask::row(position::MIN_ROW)
      : board::mask::row(position::MAX_ROW);

  board::mask const rabbits         = board.types()[index_from_type(piece::rabbit)];
  board::mask const player_pieces   = board.player(player);
  board::mask const opponent_pieces = board.player(opponent);

  if (rabbits & player_pieces & player_target)
    return player;

  if (rabbits & opponent_pieces & opponent_target)
    return opponent;

  if ((rabbits & opponent_pieces).empty())
    return player;

  if ((rabbits & player_pieces).empty())
    return opponent;

  return boost::none;
}

void killer_db::add(std::size_t level, step const& step) {
  while (level >= levels_) {
    levels_ *= 2;
    killers_.resize(levels_ * killer_count_);
  }

  killers_cont::value_type* least = 0;
  killers_cont::value_type* exact = 0;
  for (std::size_t i = 0; i < killer_count_; ++i) {
    record& r = get(level, i);
    if (r.step && *r.step == step) {
      exact = &r;
      break;
    }

    if (least == 0 || r.hits < least->hits)
      least = &r;
  }

  if (exact)
    ++exact->hits;
  else {
    assert(least);
    if (!least->step)
      ++size_;
    least->step = step;
    least->hits = 1;
  }
}

bool killer_db::is_killer(std::size_t level, step const& step) const {
  if (level < levels_)
    for (std::size_t i = 0; i < killer_count_; ++i) {
      record const& r = get(level, i);
      if (r.step) {
        if (*r.step == step)
          return true;
      } else
        break;
    }
  return false;
}

killer_db::record& killer_db::get(std::size_t level, std::size_t offset) {
  return killers_[level * killer_count_ + offset];
}

killer_db::record const& killer_db::get(std::size_t level, std::size_t offset) const {
  return killers_[level * killer_count_ + offset];
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
) const {
  // Note: Lexicographical sorting with PN/DN values as the primary key would
  // require that resort_children doesn't destroy the secondary sort order,
  // which it currently does.

  history_table::table_t::const_iterator l = table_->find(*lhs.step);
  history_table::table_t::const_iterator r = table_->find(*rhs.step);

  boost::uint64_t const left  = l != table_->end() ? l->second : 0;
  boost::uint64_t const right = r != table_->end() ? r->second : 0;

  return left > right;
}

namespace {

template <typename Iter>
void do_update_numbers(Iter begin, Iter end, vertex& v) {
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
  bool              sum_infty = false;

  for (Iter child = begin; child != end; ++child) {
    if ((*child).*minimise_num < min)
      min = (*child).*minimise_num;

    if ((*child).*sum_num < vertex::infty) {
      sum += (*child).*sum_num;
      if (sum >= vertex::infty)
        sum = vertex::infty - 1;

    } else
      sum_infty = true;

    assert(sum <= vertex::infty);
  }

  v.*minimise_num = min;
  v.*sum_num      = sum_infty ? vertex::infty : sum;

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

}

void update_numbers(vertex& v, std::size_t consider) {
  if (consider == 0 || consider >= v.size())
    do_update_numbers(v.begin(), v.end(), v);
  else {
    typedef std::vector<vertex*> children_cont;
    children_cont children;
    children.reserve(v.size());
    for (vertex::iterator c = v.begin(); c != v.end(); ++c)
      children.push_back(&*c);

    std::stable_sort(children.begin(), children.end(),
                     vertex_ptr_comparator(&v));

    do_update_numbers(boost::make_indirect_iterator(children.begin()),
                      boost::make_indirect_iterator(children.begin() + consider),
                      v);
  }
}

search_stack::search_stack(
  zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash,
  vertex* root, piece::color_t attacker, board const& initial_state
) : path_(1, root)
  , hashes_(1, initial_hash)
  , history_(1, initial_hash)
  , states_(1, initial_state)
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
  piece::color_t const          parent_player = vertex_player(*parent, attacker_);
  piece::color_t const          v_player      = vertex_player(*v, attacker_);
  zobrist_hasher::hash_t const  v_hash        = 
    v->step
      ? hasher_->update(parent_hash, v->step->begin(), v->step->end(),
                        parent_player, v_player, parent->steps_remaining)
      : hasher_->update_lambda(parent_hash, parent_player, v_player, parent->steps_remaining);

  int stage = 0;

  try {
    path_.push_back(v); ++stage;
    hashes_.push_back(v_hash); ++stage;
    if (v->type != parent->type) { history_.push_back(v_hash); ++stage; }
  } catch (...) {
    switch (stage) {
    case 3:   history_.pop_back();  // fallthrough...
    case 2:   hashes_.pop_back();   // fallthrough...
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
    states_.pop_back();
    --state_pos_;
  }

  path_.pop_back();
  hashes_.pop_back();
  if (top->type != parent->type) history_.pop_back();

  assert(!path_.empty());
  assert(!hashes_.empty());
  assert(!history_.empty());
  assert(!states_.empty());
}

void search_stack::reset_to_root() {
  while (!at_root())
    pop();
}

board const& search_stack::state() const {
  assert(state_pos_ <= path_.size() - 1);

  board state(states_.back());
  while (state_pos_ != path_.size() - 1) {
    ++state_pos_;
    if (path_[state_pos_]->step)
      apply(*path_[state_pos_]->step, state);
    states_.push_back(state);
  }

  assert(state_pos_ == path_.size() - 1);
  return states_.back();
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
      throw std::logic_error("revert: The watched stack was modified below the checkpoint");
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
  search_stack::path_sequence::const_reverse_iterator v     = stack.path().rbegin();
  search_stack::hashes_sequence::const_iterator       hash  = stack.hashes().end();

  move_history_seq result = {{}};

  vertex::e_type const type = stack.path_top()->type;
  while (v != stack.path().rend() && (**v++).type == type)
    --hash;

  assert(std::distance(hash, stack.hashes().end()) <= move_history_seq::static_size);

  search_stack::hashes_sequence::const_iterator h = hash;
  search_stack::path_sequence::const_iterator   u = 
    stack.path().end() - std::distance(hash, stack.hashes().end());
  move_history_seq::iterator dst = result.begin();

  while (h != stack.hashes().end()) {
    zobrist_hasher::hash_t const value = stack.hasher().unhash_steps(
      *h++, (**u++).steps_remaining
    );

    *dst++ = value;
  }

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
    if (child->step && child != killers_end && killers.is_killer(level, *child->step)) {
      // Need to bubble stuff around here to preserve heuristic order.
      vertex::iterator it = killers_end++;
      while (it != child)
        v.swap_children(it++, child);
    }
  }
}

std::size_t expand(vertex& leaf, board const& state, piece::color_t attacker,
                   move_history_seq const& move_hist, zobrist_hasher const& hasher) {
  assert(leaf.steps_remaining > 0);

  if (!leaf.leaf())
    throw std::logic_error("expand: Attempt to expand a non-leaf vertex");

  typedef std::vector<std::pair<steps_cont::const_iterator, vertex::e_type> > steps_seq;

  move_history_seq::const_iterator const last_it = last_iter(move_hist);
  zobrist_hasher::hash_t const last = *last_it;

  steps_seq steps;
  piece::color_t const player = vertex_player(leaf, attacker);

  steps_cont const s = generate_steps(state, player);
  for (
    steps_cont::const_iterator new_step = s.begin();
    new_step != s.end();
    ++new_step
  ) {
    int const remaining = leaf.steps_remaining - new_step->steps_used();

    // Don't generate silent steps here -- those that result in a position that is the same
    // as one previously encountered within this move. For that, we want to ignore the player
    // and steps remaining.
    
    zobrist_hasher::hash_t child_hash = hasher.update_no_steps(
      last, new_step->begin(), new_step->end(), player, player
    );

    if (std::find(move_hist.begin(), move_hist.end(), child_hash) == move_hist.end()) {
      if (remaining == 0)
        steps.push_back(std::make_pair(new_step, opposite_type(leaf.type)));
      else if (remaining > 0)
        steps.push_back(std::make_pair(new_step, leaf.type));
    }
  }

  if (!steps.empty()) {
    leaf.resize(steps.size() + 1);  // + lambda vertex.

    vertex::iterator          child = leaf.begin();
    steps_seq::const_iterator step  = steps.begin();

    for (; child != leaf.end(); ++child) {
      child->proof_number = child->disproof_number = 1;
      child->subtree_size = 1;

      if (step != steps.end()) {
        child->step = *step->first;
        child->type = step->second;
        ++step;

        if (child->type == leaf.type)
          child->steps_remaining = leaf.steps_remaining - child->step->steps_used();
        else
          child->steps_remaining = MAX_STEPS;

      } else {
        // Lambda step.
        if (leaf.steps_remaining - 1 > 0) {
          child->type = leaf.type;
          child->steps_remaining = leaf.steps_remaining - 1;
        } else {
          child->type = opposite_type(leaf.type);
          child->steps_remaining = MAX_STEPS;
        }
      }
    }
  }

  return leaf.size();
}

void evaluate(search_stack& stack, piece::color_t attacker, log_sink& log) {
  vertex& child = *stack.path_top();
  vertex const* const parent = *boost::next(stack.path().rbegin());

  piece::color_t const player = vertex_player(*parent, attacker);

  boost::optional<piece::color_t> const winner = apns::winner(stack.state(), player);

  if (winner) {
    // If this isn't the start of a move, only consider wins, not losses.

    if (child.type != parent->type || *winner == player) {
      if (*winner == attacker) {
        child.proof_number = 0;
        child.disproof_number = vertex::infty;
      } else {
        child.proof_number = vertex::infty;
        child.disproof_number = 0;
      }

      log << stack
          << ' ' << (stack.path_top()->proof_number == 0 ? "proved" : "disproved")
          << " by evaluation function\n";

      return;
    }
  }
}

bool pt_lookup(proof_table& pt, search_stack& stack, piece::color_t attacker) {
  vertex& child = *stack.path_top();
  boost::optional<proof_table::entry_t const&> const values = pt.query(stack.hashes_top());

  if (values && histories_compatible(stack, values->history)) {
    if (values->winner == attacker) {
      child.proof_number    = 0;
      child.disproof_number = vertex::infty;
    } else {
      child.proof_number    = vertex::infty;
      child.disproof_number = 0;
    }

    return true;
  } else if (values) {
    pt.reject();
  }

  return false;
}

void pt_store(proof_table& pt, vertex const& v,
              zobrist_hasher::hash_t hash, piece::color_t attacker,
              search_stack::history_sequence::const_iterator history_begin,
              search_stack::history_sequence::const_iterator history_end) {
  assert(v.proof_number == 0 || v.disproof_number == 0);

  proof_entry_t entry(
    v.proof_number == 0 ? attacker : opponent_color(attacker),
    history_t(std::distance(history_begin, history_end))
  );
  std::copy(history_begin, history_end, entry.history.begin());
  pt.insert(hash, v.subtree_size, entry);
}

bool tt_lookup(transposition_table& tt, zobrist_hasher::hash_t hash, vertex& child) {
  boost::optional<transposition_table::entry_t const&> values = tt.query(hash);

  if (values) {
    assert(values->proof_number > 0 && values->disproof_number > 0);

    child.proof_number = values->proof_number;
    child.disproof_number = values->disproof_number;

    return true;
  }

  return false;
}

void tt_store(transposition_table& tt, vertex const& v, zobrist_hasher::hash_t hash) {
  assert(v.proof_number > 0 && v.disproof_number > 0);
  tt.insert(hash, v.subtree_size, transposition_entry(v.proof_number, v.disproof_number));
}

void garbage_collect(std::size_t how_many, search_stack const& orig_stack) {
  search_stack stack(orig_stack);
  stack.reset_to_root();
  vertex* const root = stack.path_top();

  if (root->proof_number == 0 || root->disproof_number == 0)
    return;

  std::size_t removed = 0;
  while (removed < how_many) {
    vertex const* const avoid = stack.size() <= orig_stack.size() ? orig_stack.path()[stack.size() - 1] : 0;
    vertex* const avoid_child = stack.size() + 1 <= orig_stack.size() ? orig_stack.path()[stack.size()] : 0;

    removed += collect_proved(*stack.path_top(), avoid_child);
    
    vertex& current = *stack.path_top();
    if (!current.leaf()) {
      vertex_comparator worse(current, false);
      vertex::reverse_iterator worst = current.rend();

      bool want_avoiding = false;

      for (vertex::reverse_iterator child = current.rbegin(); child != current.rend(); ++child) {
        if (&*child != avoid_child) {
          if (!child->leaf() && (worst == current.rend() || worse(*child, *worst)))
            worst = child;
        } else
          want_avoiding = true;
      }

      if (worst == current.rend() && want_avoiding)
        stack.push(avoid_child);
      else if (worst != current.rend())
        stack.push(&*worst);
      else if (!stack.at_root()) {
        if (stack.path_top() != avoid) {
          std::size_t const removed_here = cut(*stack.path_top());
          for (
            search_stack::path_sequence::const_reverse_iterator v = stack.path().rbegin();
            v != stack.path().rend();
            ++v
          )
            (**v).subtree_size -= removed_here;

          removed += removed_here;
          stack.pop();
        } else {
          break;
        }
      } else
        break;

    } else {
      if (!stack.at_root())
        stack.pop();
      else
        break;
    }
  }
}

std::size_t collect_proved(vertex& parent, vertex const* avoid) {
  std::size_t removed = 0;

  for (vertex::iterator child = parent.begin(); child != parent.end(); ++child) {
    if (&*child != avoid && child->type != parent.type &&
        ((parent.type == vertex::type_or && child->disproof_number == 0) ||
         (parent.type == vertex::type_and && child->proof_number == 0))) {
      removed += cut(*child);
    }
  }
  
  return removed;
}

std::size_t cut(vertex& parent) {
  namespace bl = boost::lambda;
  std::size_t const count = std::accumulate(
    parent.begin(), parent.end(), (std::size_t) 0,
    bl::ret<std::size_t>(bl::_1 + bl::bind(&vertex::subtree_size, bl::_2))
  );

  parent.resize(0);
  parent.pack();

  return count;
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
    int remaining = step ? parent.steps_remaining - step->steps_used() : 0;
    if (step && remaining >= 0) {
      assert(parent.leaf());

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

      bool const found_in_pt = proof_tbl && pt_lookup(*proof_tbl, stack, attacker);
      if (!found_in_pt)
        evaluate(stack, attacker, log);

      if (cutoff(parent, *child)) {
        log << stack
            << ' ' << (stack.path_top()->proof_number == 0 ? "proved" : "disproved")
            << " by simulation\n";

        update_numbers(parent);
        assert(parent.proof_number == 0 || parent.disproof_number == 0);

        child_added.commit();

        return true;
      }

#if KILLER_SIMULATE_RECURSIVE
      else if (remaining >= 1) {
        child->type = parent.type;
        child->steps_remaining = remaining;

        if (simulate(stack, attacker, size, killers, proof_tbl, log)) {
          log << stack
              << ' ' << (stack.path_top()->proof_number == 0 ? "proved" : "disproved")
              << " by recursive simulation\n";

          update_numbers(parent);

          assert(child->proof_number == 0 || child->disproof_number == 0);
          assert(parent.proof_number == 0 || parent.disproof_number == 0);

          child_added.commit();

          return true;
        }
      }
#endif
    }
  }

  assert(parent.leaf());
  return false;
}

//! Check whether the game would be lost due to a repetition if the
//! given player made the given step from the given position assuming the
//! passed-in game history. This may only be used on the first step in a move.
bool repetition(search_stack const& stack) {
  assert(stack.path_top()->steps_remaining == MAX_STEPS);

  if (stack.history().size() >= 2) {
    // Check for null moves.
    if (stack.hasher().opponent_hash(stack.history().back()) == *(stack.history().end() - 2))
      return true;

    // Check for third-time repetitions.
    // Only check against the last element in .history() as the previous ones
    // should have been checked by previous calls to this function.

    unsigned rep_count = 0;
    for (
      search_stack::history_sequence::const_iterator h = stack.history().begin(); h != stack.history().end() - 1; ++h
    )
      if (*h == stack.history().back() && ++rep_count == 2)  // Twice in the history, third time is the .top().
        return true;
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

  while (!stack_.path_top()->leaf()) {
    select_best();
  }

  assert(stack_.path_top()->proof_number > 0 && stack_.path_top()->disproof_number > 0);

  vertex&             current = *stack_.path_top();
  vertex const* const parent  = apns::parent(stack_);

  bool const found_in_pt =
    proof_tbl_ && parent && current.type != parent->type &&
    pt_lookup(*proof_tbl_, stack_, game_->attacker);

  std::size_t increment = 0;
  if (!found_in_pt)
    increment = expand_and_eval();
  else
    stack_.pop();

  search_stack::path_sequence::const_reverse_iterator    path_current    = stack_.path().rbegin();
  search_stack::hashes_sequence::const_reverse_iterator  hash_current    = stack_.hashes().rbegin();
  search_stack::history_sequence::const_reverse_iterator history_current = stack_.history().rbegin();

  assert(path_current != stack_.path().rend());

  bool changed = true;
  while (true) {
    vertex& current = **path_current;

    vertex* parent = 0;
    if (boost::next(path_current) != stack_.path().rend())
      parent = *(boost::next(path_current));

    changed = update_and_store(
      current, parent,
      stack_.path().begin(), path_current.base(),
      stack_.history().begin(), history_current.base(),
      *hash_current, increment, changed
    );

    if (parent) {
      if (changed) {
        stack_.pop();
        path_current = stack_.path().rbegin();
        hash_current = stack_.hashes().rbegin();
        history_current = stack_.history().rbegin();
      } else {
        ++path_current;
        ++hash_current;

        if (current.type != parent->type)
          ++history_current;
      }
    } else
      break;
  }
}

void depth_first_pns::do_iterate() {
  assert(game_);
  assert(!game_->root.step);
  assert(limits_.size() == stack_.size());
  assert(!finished());
  assert(log_);

  // Go up while we're at a vertex whose numbers are off-limits.
  vertex* current = stack_.path_top();
  std::size_t cut_count = 0;
  while (!stack_.at_root() &&
         (current->proof_number == 0 || 
          current->proof_number > limits_.back().pn_limit ||
          current->disproof_number == 0 || 
          current->disproof_number > limits_.back().dn_limit)) {
    bool const cut = !gc_enabled() && current->type != parent(stack_)->type;
    if (cut)
      cut_count += apns::cut(*current);
    assert(cut_count < current->subtree_size || current->subtree_size == 0);
    current->subtree_size -= cut_count;

    stack_.pop();
    limits_.pop_back();
    current = stack_.path_top();

    assert(!limits_.empty());
  }

  // Finish iterating until root to update .subtree_size's.
  for (search_stack::path_sequence::const_reverse_iterator v = stack_.path().rbegin(); v != stack_.path().rend(); ++v)
    (**v).subtree_size -= cut_count;

  assert(current->proof_number > 0 && current->disproof_number > 0);
  assert(current->proof_number <= limits_.back().pn_limit &&
         current->disproof_number <= limits_.back().dn_limit);

  // And go back down until we reach a leaf.
  while (!current->leaf()) {
#if KILLER_SORT_BEFORE_SELECT
    if (killer_db_)
      killer_order(*killer_db_, virtual_level(stack_), *stack_.path_top());
#endif

#if KILLER_PREFER
    std::pair<vertex*, vertex*> best_two =
      killer_db_
        ? two_best_successors(*current, killer_vertex_value(virtual_level(stack_), *killer_db_))
        : two_best_successors(*current);
#else
    std::pair<vertex*, vertex*> best_two = two_best_successors(*current);
#endif

    assert(best_two.first);
    assert(best_two.first->proof_number > 0 && best_two.first->disproof_number > 0);

    if (best_two.second) {
      if (current->type == vertex::type_or) {
        assert(best_two.second->proof_number > 0);
      } else {
        assert(best_two.second->disproof_number > 0);
      }
    }

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

  assert(current->leaf());
  assert(current->proof_number > 0 && current->disproof_number > 0);

  vertex const* parent  = apns::parent(stack_);

  bool found_in_pt =
    proof_tbl_ && parent && current->type != parent->type &&
    pt_lookup(*proof_tbl_, stack_, game_->attacker);
  std::size_t increment = 0;
  if (!found_in_pt)
    increment = expand_and_eval();

  search_stack::path_sequence::const_reverse_iterator
    current_path = stack_.path().rbegin();
  search_stack::hashes_sequence::const_reverse_iterator
    current_hash = stack_.hashes().rbegin();
  search_stack::history_sequence::const_reverse_iterator
    current_history = stack_.history().rbegin();
  
  bool changed = true;

  while (true) {
    vertex* parent = 0;
    if (boost::next(current_path) != stack_.path().rend())
      parent = *boost::next(current_path);

    if (!found_in_pt) {
      changed = update_and_store(
        *current, parent,
        stack_.path().begin(), current_path.base(),
        stack_.history().begin(), current_history.base(),
        *current_hash, increment, changed
      );
    } else {
      found_in_pt = false;
    }

    if (parent) {
      if (parent->type != current->type) {
        ++current_history;
        assert(current_history != stack_.history().rend());
      }

      current = *++current_path;
      ++current_hash;
    } else {
      break;
    }
  }
}

depth_first_pns::limits_t 
depth_first_pns::make_limits(
  vertex const& v, vertex const& parent,
  boost::optional<vertex const&> second_best,
  limits_t parent_limits
) {
  double const EPSILON = 0.9;

  vertex::number_t vertex::* min_num = 
    parent.type == vertex::type_or ? &vertex::proof_number
                                   : &vertex::disproof_number;
  vertex::number_t limits_t::* min_lim =
    parent.type == vertex::type_or ? &limits_t::pn_limit
                                   : &limits_t::dn_limit;
  vertex::number_t vertex::* dif_num = 
    parent.type == vertex::type_or ? &vertex::disproof_number
                                   : &vertex::proof_number;
  vertex::number_t limits_t::* dif_lim =
    parent.type == vertex::type_or ? &limits_t::dn_limit
                                   : &limits_t::pn_limit;

  limits_t new_limits;

  vertex::number_t second_num = 
    second_best ? (*second_best).*min_num : v.*min_num;
  assert(second_num > 0);

  double sn = std::ceil(second_num * (1 + EPSILON));
  if (sn < vertex::infty - 1.5)
    new_limits.*min_lim = std::min(
      parent_limits.*min_lim,
      //second_num + 1
      static_cast<vertex::number_t>(sn)
    );
  else
    new_limits.*min_lim = vertex::infty - 1;

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

