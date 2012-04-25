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

namespace {

//! Check whether the game would be lost if the given player made the given
//! step from the given position assuming the passed-in ! game history.
bool repetition(apns::search_stack const& stack) {
  using namespace apns;

  if (stack.history().size() >= 2) {
    if (stack.hasher().opponent_hash(stack.history().back()) == 
        *(stack.history().end() - 2))
      return true;  // Loss because the last player's move has not resulted in 
                    // a net change in game position.

    // Check for third-time repetitions. Only check against the last element
    // in .history() as the previous ones should have been checked by previous
    // calls to this function.

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

apns::piece::color_t vertex_player(apns::vertex const& v, 
                                   apns::piece::color_t attacker) {
  return v.type == apns::vertex::type_or ? 
    attacker : apns::opponent_color(attacker);
}

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
    player == piece::gold ? board::MAX_ROW : board::MIN_ROW;
  position::row_t const opponents_target_row =
    player == piece::gold ? board::MIN_ROW : board::MAX_ROW;

  bool player_has_rabbits = false;
  bool opponent_has_rabbits = false;

  for (position::col_t col = board::MIN_COLUMN; col <= board::MAX_COLUMN;
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

  for (position::col_t col = board::MIN_COLUMN; col <= board::MAX_COLUMN;
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

    for (position::row_t row = board::MIN_ROW + 1; row <= board::MAX_ROW - 1;
         ++row) {  // We've already cheched first and last row.
      for (position::col_t col = board::MIN_COLUMN; col <= board::MAX_COLUMN; 
           ++col) {
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
  vertex::children_iterator best = std::min_element(
    parent.children_begin(), parent.children_end(),
    vertex_comparator(parent)
  );

  if (best != parent.children_end())
    return &*best;
  else
    return 0;
}

vertex const* best_successor(vertex const& parent) {
  vertex::const_children_iterator best = std::min_element(
    parent.children_begin(), parent.children_end(),
    vertex_comparator(parent)
  );

  if (best != parent.children_end())
    return &*best;
  else
    return 0;
}

std::pair<vertex const*, vertex const*>
two_best_successors(vertex const& parent) {
  if (parent.children_count() >= 2) {
    vertex::const_children_iterator best = parent.children_begin();
    vertex::const_children_iterator second_best =
      boost::next(parent.children_begin());
    vertex_comparator better(parent);

    if (better(*second_best, *best))
      std::swap(best, second_best);

    for (vertex::const_children_iterator child = boost::next(second_best);
         child != parent.children_end(); ++child) {
      if (better(*child, *best)) {
        second_best = best;
        best = child;
      } else if (better(*child, *second_best) && child != best)
        second_best = child;
    }

    assert(best != second_best);

    return std::make_pair(&*best, &*second_best);
  } else {
    return std::make_pair(best_successor(parent),
                          static_cast<vertex const*>(0));
  }
}

void killer_db::add(std::size_t ply, vertex::e_type type, step const& step) {
  plys& p = get_plys(type);
  if (ply >= p.size())
    p.resize(ply + 1, ply_killers_t(killer_count_));

  if (std::find(p[ply].begin(), p[ply].end(), step) == p[ply].end())
    p[ply].push_back(step);
}

bool killer_db::is_killer(
  std::size_t ply, vertex::e_type type, step const& step
) const {
  for (ply_iterator killer = ply_begin(ply, type);
       killer != ply_end(ply, type); ++killer)
    if (*killer == step)
      return true;

  return false;
}

void history_table::insert(step const& step, std::size_t depth) {
  table_t::mapped_type const value = 1 << depth;

  table_t::iterator record = table_.find(step);
  if (record != table_.end()) {
    if (record->second < value)
      record->second = value;
  } else {
    table_[step] = value;
  }
}

void history_table::sort(vertex& v) {
  v.sort_children(compare(table_));
}

bool history_table::compare::operator () (
  vertex const& lhs, vertex const& rhs
) {
  history_table::table_t::iterator l = table->find(*lhs.step);
  history_table::table_t::iterator r = table->find(*rhs.step);

  boost::uint64_t const left = l != table->end() ? l->second : 0;
  boost::uint64_t const right = r != table->end() ? r->second : 0;

  return left < right;
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
  //
  // This magic constant sucks. But C++03 lacks constexpr...
  typedef boost::uint_value_t<8589934590>::fast sum_num_t; 

  vertex::number_t  min = vertex::infty;
  sum_num_t         sum = 0;

  for (vertex::children_iterator child = v.children_begin();
       child != v.children_end(); ++child) {
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
  assert(v->step);
  assert(hasher_);

  vertex* const                 parent        = path_.back();
  zobrist_hasher::hash_t const  parent_hash   = hashes_.back();
  piece::color_t const          parent_player =
    vertex_player(*parent, attacker_);
  piece::color_t const          v_player      = vertex_player(*v, attacker_);
  zobrist_hasher::hash_t const  v_hash        = hasher_->update(
    parent_hash,
    v->step->step_sequence_begin(), v->step->step_sequence_end(),
    parent_player, v_player
  );

  int stage = 0;

  try {
    path_.push_back(v); ++stage;
    hashes_.push_back(v_hash); ++stage;
    //apply(*v->step, state_); ++stage;

    if (v->type != parent->type) {
      history_.push_back(v_hash);
      ++stage;
    }
  } catch (...) {
    switch (stage) {
    case 3:   history_.pop_back();
    //case 3:   unapply(*v->step, state_);
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
  assert(hashes_.size() >= 2);

  vertex* const top = path_.back();
  vertex* const parent = *(path_.end() - 2);

  assert(top->step);

  if (state_pos_ == path_.size() - 1) {
    unapply(*top->step, state_);
    --state_pos_;
  }

  path_.pop_back();
  hashes_.pop_back();

  if (top->type != parent->type)
    history_.pop_back();
}

board const& search_stack::state() const {
  assert(state_pos_ <= path_.size() - 1);

  while (state_pos_ != path_.size() - 1) {
    ++state_pos_;
    apply(*path_[state_pos_]->step, state_);
  }

  assert(state_pos_ == path_.size() - 1);
  return state_;
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

boost::optional<move_path>
player_moves::query(zobrist_hasher::hash_t hash, 
                    board const& expected_position) {
  moves_map::iterator result = moves_.find(hash);
  if (result != moves_.end() && verify(result->second, expected_position)) {
    return result->second;
  } else {
    return boost::none;
  }
}

bool player_moves::verify(move_path const& path, board const& expected) {
  if (try_apply(path, root_position_)) {
    bool const matches = root_position_ == expected;
    unapply(path, root_position_);
    return matches;
  } else {
    return false;
  }
}

boost::shared_ptr<player_moves> 
moves::get(zobrist_hasher::hash_t root_hash, board const& root_pos,
           vertex* root, zobrist_hasher const& hasher, piece::color_t attacker) 
{
  ++time_;

  records_map::iterator record = records_.find(root_hash);
  if (record != records_.end() && record->second.moves && 
      record->second.moves->root_position() == root_pos) {
    ++hits_;
    record->second.last_used = time_;
    return record->second.moves;
  }
  else {
    ++misses_;

    // Not found. We'll have to generate one.
    boost::shared_ptr<player_moves> moves(new player_moves(root_pos));
    search_stack stack(hasher, root_hash, root, attacker, root_pos);
    gather(moves, stack);

    records_.insert(std::make_pair(root_hash, move_record(time_, moves)));

    if (records_.size() > moves_cached_)
      resize_records(moves_cached_);

    return moves;
  }
}

void moves::remove(zobrist_hasher::hash_t root_hash) {
  records_.erase(root_hash);
}

void moves::moves_cached(std::size_t new_moves_cached) {
  resize_records(new_moves_cached);
  moves_cached_ = new_moves_cached;
}

void moves::gather(boost::shared_ptr<player_moves> const& moves,
                   search_stack& stack) {
  vertex* const current = stack.path_top();

  for (vertex::children_iterator child = current->children_begin();
       child != current->children_end(); ++child) {
    search_stack_checkpoint checkpoint(stack);
    stack.push(&*child);

    if (child->type != stack.path().front()->type) {
      // This is a move terminal. Insert the child into moves and do not 
      // recurse into it.
      move_path child_path = {{}};
      assert(stack.path().size() <= 5);
      std::copy(stack.path().begin() + 1, stack.path().end(),
                child_path.begin());
      moves->insert(stack.hashes_top(), child_path);
    }
    else {
      // This is a move-inner vertex -- recurse into it.
      gather(moves, stack);
    }
  }
}

void moves::resize_records(std::size_t new_size) {
  while (records_.size() > new_size) {
    records_map::iterator to_remove = std::min_element(
      records_.begin(), records_.end(),
      boost::bind(&moves::records_compare, this, _1, _2)
    );
    assert(to_remove != records_.end());
    records_.erase(to_remove);
  }
}

bool moves::records_compare(records_map::value_type const& lhs, 
                            records_map::value_type const& rhs) {
  //return lhs.second.last_used < rhs.second.last_used;
  return lhs.second.moves->size() < rhs.second.moves->size();
}

void search_tree::expand() {
  vertex* const leaf = stack_.path_top();
  if (!leaf->leaf())
    throw std::logic_error("expand: Attempt to expand a non-leaf vertex");

  // Make a list of all possible steps.
  typedef std::vector<std::pair<step, vertex::e_type> > steps_seq;
  steps_seq steps;

  piece::color_t const player = vertex_player(*leaf, attacker_);
  for (all_steps_iter new_step = all_steps_begin(stack_.state(), player);
       new_step != all_steps_end(); ++new_step) {
    int const remaining =
      leaf->steps_remaining - static_cast<signed>(new_step->steps_used());

    if (remaining >= 0) {
      steps.push_back(std::make_pair(*new_step, opposite_type(leaf->type)));
      if (remaining >= 1)
        steps.push_back(std::make_pair(*new_step, leaf->type));
    }
  }

  leaf->resize(steps.size());

  vertex::children_iterator child = leaf->children_begin();
  for (steps_seq::const_iterator s = steps.begin(); s != steps.end(); 
       ++s, ++child) {
    child->step = s->first;
    child->type = s->second;

    if (child->type == leaf->type)
      child->steps_remaining =
        leaf->steps_remaining - static_cast<signed>(child->step->steps_used());
    else
      child->steps_remaining = MAX_STEPS;

    child->proof_number = 1;
    child->disproof_number = 1;
  }

  size_ += leaf->children_count();

  history_->sort(*leaf);
}

void search_tree::evaluate() {
  vertex& child = *stack_.path_top();
  vertex const* const parent = *boost::prior(stack_.path().rend());

  if (!evaluate_cached()) {
    piece::color_t const player = vertex_player(*parent, attacker_);
    boost::optional<piece::color_t> winner;

    // Only check for win if this is the start of a move and if a rabbit was
    // moved or captured in this move.
    if (child.type != parent->type) {
      search_stack::path_sequence::const_reverse_iterator iter =
        stack_.path().rbegin();

      do {
        vertex* const current = *iter;
        if (current->step && current->step->moves_rabbit()) {
          winner = apns::winner(stack_.state(), player);
          break;
        }

        ++iter;
      } while (iter != stack_.path().rend() && (**iter).type == parent->type);
    }

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
      if (child.type != parent->type && repetition(stack_)) {
        child.proof_number = player == attacker_ ? vertex::infty : 0;
        child.disproof_number = player == attacker_ ? 0 : vertex::infty;
      } else {
        child.proof_number = 1;
        child.disproof_number = 1;
      }
    }
  }
}

void search_tree::evaluate_children() {
  vertex& current = *stack_.path_top();
  for (vertex::children_iterator child = current.children_begin();
       child != current.children_end(); ++child) {
    search_stack_checkpoint checkpoint(stack_);
    stack_.push(&*child);
    evaluate();
  }
}

bool search_tree::evaluate_cached() {
  vertex& child = *stack_.path_top();

  if (proof_tbl_) {
    boost::optional<proof_table::entry_t> values = 
      proof_tbl_->query(stack_.hashes_top());

    if (values && histories_compatible(stack_, values->history)) {
      child.proof_number = values->proof_number;
      child.disproof_number = values->disproof_number;

      return true;
    } else if (values) {
      proof_tbl_->reject();
    }
  }

  if (trans_tbl_) {
    // Values weren't found in the proof table.
    boost::optional<transposition_table::entry_t> values = 
      trans_tbl_->query(stack_.hashes_top());

    if (values) {
      child.proof_number = values->proof_number;
      child.disproof_number = values->disproof_number;

      return true;
    }
  }

  return false;
}

void search_tree::cut_children() {
  vertex& parent = *stack_.path_top();

  vertex_counter cut_counter;
  traverse(parent, backtrack(), boost::ref(cut_counter));

  if (moves_.size() > 0) {
    vertex*                 root;
    zobrist_hasher::hash_t  root_hash;
    board                   root_position;
    std::size_t             move_len;
    find_move_root(root, root_hash, root_position, move_len);

    // Simply remove the whole move from the DB. It will be regenerated 
    // later on.
    moves_.remove(root_hash);
  }

  parent.resize(0);
  parent.pack();

  size_ -= cut_counter.count - 1;  // parent has been counted but not removed.
}

void search_tree::reduce() {
  vertex*                 root;
  zobrist_hasher::hash_t  root_hash;
  board                   root_position;
  std::size_t             current_depth;
  find_move_root(root, root_hash, root_position, current_depth);

  boost::shared_ptr<player_moves> moves = moves_.get(
    root_hash,
    root_position,
    root,
    *hasher_,
    attacker_
  );

  // Children that are meant to be transferred to the current one will
  // temporarily be stored in transferred_children.  Transferring them
  // directly would result in invalidating the iterators in the sequence
  // that's currently being iterated over. That wouldn't be nice.
  //
  // For the sake of genericity, let's not assume that some sort of container
  // is going to be compatible with vertex -- just store the children directly
  // in a vertex. It's a slight abusal, but it ought to work.

  vertex transferred_children;
  vertex& parent = *stack_.path().back();

  vertex::children_iterator child = parent.children_begin();
  while (child != parent.children_end()) {
    if (child->type != parent.type) {
      search_stack_checkpoint checkpoint(stack_);
      stack_.push(&*child);

      boost::optional<move_path> move =
        moves->query(stack_.hashes_top(), stack_.state());

      if (move) {
        // A pre-existing move was found. The question now is: Is this
        // pre-existing move at shallower depth?

        std::size_t other_depth = move_depth(*move);
        if (other_depth <= current_depth + 1) {
          // It is -- we want to keep the other and discard this one.
          stack_.pop();
          child = parent.remove_child(child);
          --size_;
        }
        else {
          // This is the shallower child -- means we're removing the other one
          // and keeping this one.

          moves->remove(stack_.hashes_top());
          move_path::iterator other_iter = terminal(*move);
          vertex* other = *other_iter--;
          vertex* other_parent = *other_iter;

          if (other_iter != move->begin()) --other_iter;

          // Steps don't need to be necessarily compatible.
          other->step = child->step;

          transferred_children.transfer_child(
            *other_parent, other_parent->iter_from_ptr(other)
          );

          // If we removed all children of the other vertex, remove the other
          // vertex as well.
          while (other_parent->leaf()) {
            other = other_parent;
            other_parent = *other_iter;
            other_parent->remove_child(other_parent->iter_from_ptr(other));

            if (other_iter != move->begin())
              --other_iter;
            else
              break;
          }

          other_parent->pack();
          update_numbers(*other_parent);

          // Update numbers along the path from other parent to move-root.
          while (other_iter != move->begin()) {
            update_numbers(**--other_iter);
          }

          // Remove child as we're going to transfer the found one to the
          // parent. Since child is going to be removed -- that could hurt when
          // popping it from the stack.
          stack_.pop();  
          child = parent.remove_child(child);
        }
      }
      else {
        // Not found. So let's add this one to the moves database.
        store_move(moves, current_depth + 1);
        ++child;
      }
    } else {
      ++child;
    }
  }

  // Now finish the transfer of children -- move them from transferred children
  // really to the target vertex.
  while (transferred_children.children_count() > 0) {
    search_stack_checkpoint checkpoint(stack_);
    stack_.push(&*transferred_children.children_begin());

    parent.transfer_child(
      transferred_children, transferred_children.children_begin()
    );
    store_move(moves, current_depth + 1);
  }

  parent.pack();
}

void search_tree::update_path() {
  search_stack::path_sequence::const_reverse_iterator path_current =
    stack_.path().rbegin();
  search_stack::hashes_sequence::const_reverse_iterator hashes_current =
    stack_.hashes().rbegin();
  search_stack::history_sequence::const_reverse_iterator  history_current =
    stack_.history().rbegin();
  std::size_t ply = stack_.history().size();

  while (true) {
    vertex& current = **path_current;

    update_numbers(current);

    if (history_ && boost::next(path_current) != stack_.path().rend()) {
      vertex const& parent = **(boost::next(path_current));
      if ((parent.type == vertex::type_or && current.proof_number == 0) ||
          (parent.type == vertex::type_and && current.disproof_number == 0)) {
        history_->insert(*current.step, ply);
      }

      if (proof_tbl_ && 
          (current.proof_number == 0 || current.disproof_number == 0)) {
        proof_entry_t entry(
          current.proof_number, current.disproof_number,
          history_t(std::distance(history_current, stack_.history().rend()))
        );
        std::copy(history_current, stack_.history().rend(),
                  entry.history.rbegin());
        proof_tbl_->insert(*hashes_current, ply, entry);
      }

      if (trans_tbl_ && 
          (current.proof_number != 0 && current.disproof_number != 0)) {
        trans_tbl_->insert(
          *hashes_current, ply,
          transposition_entry(current.proof_number, current.disproof_number)
        );
      }
    }

    search_stack::path_sequence::const_reverse_iterator parent =
      boost::next(path_current);
    if (parent != stack_.path().rend()) {
      if (current.type != (**parent).type) {
        ++history_current;
        --ply;
      }

      ++hashes_current;
      ++path_current;
    } else {
      break;
    }
  }
}

void search_tree::find_move_root(
  vertex*& root, zobrist_hasher::hash_t& root_hash, board& position,
  std::size_t& move_len
) {
  search_stack::path_sequence::const_reverse_iterator current =
    stack_.path().rbegin();
  search_stack::hashes_sequence::const_reverse_iterator current_hash =
    stack_.hashes().rbegin();

  root      = *current;
  root_hash = *current_hash;
  position  = stack_.state();
  move_len  = 0;

  ++current;
  ++current_hash;
  while (current != stack_.path().rend() && 
         (**current).type == stack_.path_top()->type) {
    if (root->step)
      unapply(*root->step, position);

    root      = *current++;
    root_hash = *current_hash++;
    ++move_len;
  }

  assert(move_len <= 3);
}

void search_tree::store_move(boost::shared_ptr<player_moves> const& moves, 
                             std::size_t current_depth) {
  assert(current_depth <= 4);
  assert(stack_.path().size() >= current_depth);

  move_path path = {{}};
  search_stack::path_sequence::const_iterator v =
    stack_.path().end() - current_depth;
  for (std::size_t d = 0; d < current_depth; ++d, ++v) {
    path[d] = *v;
  }

  moves->insert(stack_.hashes_top(), path);
}

void proof_number_search::do_iterate() {
  assert(game_);
  assert(!game_->root.step);

  tree_.select_root();

  while (!tree_.current().leaf()) {
    select_best(tree_);
  }

  tree_.expand();
  tree_.evaluate_children();
  tree_.update_path();
}

void depth_first_pns::do_iterate() {
  assert(game_);
  assert(!game_->root.step);
  assert(limits_.size() == tree_.selection_depth());
  assert(!finished());

  // Go up while we're at a vertex whose numbers are off-limits.
  while (!tree_.at_root() &&
         (tree_.current().proof_number == 0 || 
          tree_.current().proof_number > limits_.back().pn_limit ||
          tree_.current().disproof_number == 0 || 
          tree_.current().disproof_number > limits_.back().dn_limit)) {
    if (tree_.current().type != tree_.parent().type)
      tree_.cut_children();

    tree_.select_parent();
    limits_.pop_back();

    assert(!limits_.empty());
  }

  assert(tree_.current().proof_number > 0 &&
         tree_.current().disproof_number > 0);

  // And go back down until we reach a leaf.
  while (!tree_.current().leaf()) {
    std::pair<vertex const*, vertex const*> best_two =
      two_best_successors(tree_.current());

    assert(best_two.first);
    assert(best_two.first->proof_number > 0 && 
           best_two.first->disproof_number > 0);

    limits_.push_back(make_limits(
      *best_two.first,
      tree_.current(),
      best_two.second ? 
        boost::optional<vertex const&>(*best_two.second) : boost::none,
      limits_.back()
    ));
    tree_.select_child(best_two.first);

    assert(tree_.current().proof_number <= limits_.back().pn_limit);
    assert(tree_.current().disproof_number <= limits_.back().dn_limit);
  }

  tree_.expand();
  tree_.evaluate_children();
  tree_.update_path();
}

depth_first_pns::limits_t 
depth_first_pns::make_limits(
  vertex const& v, vertex const& parent,
  boost::optional<vertex const&> second_best,
  limits_t parent_limits
) {
  double const EPSILON = 0.1;

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
    second_best ? (*second_best).*min_num : vertex::infty;
  new_limits.*min_lim = std::min(
    parent_limits.*min_lim,
    static_cast<vertex::number_t>(std::ceil(second_num * (1 + EPSILON)))
  );

  if (parent_limits.*dif_lim < vertex::infty)
    new_limits.*dif_lim = 
      parent_limits.*dif_lim - parent.*dif_num + v.*dif_num;
  else
    new_limits.*dif_lim = vertex::infty;

  return new_limits;
}

} // namespace apns

