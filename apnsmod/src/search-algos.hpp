#ifndef SEARCH_ALGOS_HPP
#define SEARCH_ALGOS_HPP

#include "tree.hpp"
#include "hash.hpp"
#include "movement.hpp"

#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/utility.hpp>
#include <boost/timer.hpp>
#include <boost/ref.hpp>
#include <boost/circular_buffer.hpp>
#include <boost/bind.hpp>

#include <vector>
#include <stack>
#include <cassert>

namespace apns {

/**
 * Did any player win?
 *
 * \param board The game situation.
 * \param player Last player to have made a move.
 * \return If any player has won, return their color; otherwise return nothing.
 */
boost::optional<piece::color_t> winner(board const& board, piece::color_t player);

//! Return the best successor of a vertex. Can be used as a traversal policy.
vertex::children_iterator best_successor(vertex& parent);

//! Return the best successor of a vertex and, if any, the second-best successor.
std::pair<vertex::children_iterator, vertex::children_iterator> two_best_successors(vertex& parent);

//! A no-op visitor for board_visitor.
struct null_board_visitor {
  void operator () (vertex const&, board const&) { }
};

//! A stack of boards visited during tree traversal. This doesn't actually store the whole stack but instead only the current
//! board and updates it.
struct board_stack {
  explicit board_stack(apns::board const& initial_board) :
    board(initial_board)
  { }

  apns::board const& top() const {
    return board;
  }

  void push(step const& s) {
    apply(s, board);
  }

  void pop(step const& s) {
    unapply(s, board);
  }

private:
  apns::board board;
};

//! Container for the history of a path. That is, all board states at the point of the beginnings of each player's turn.
struct history_stack {
  struct record {
    apns::board       position;
    piece::color_t    player;

    record(apns::board const& b, piece::color_t p) : position(b), player(p) { }
  };

  typedef std::vector<record> records_cont;

  explicit history_stack(piece::color_t attacker);

  records_cont const& records() const { return current_records; }

  void push(vertex const& vertex, board const& state);
  void pop(vertex const& vertex);

private:
  records_cont    current_records;
  vertex::e_type  last_visited_type;
  piece::color_t  attacker;
};

//! History of hashes met during a traversal.
struct hashes_stack {
  typedef std::vector<zobrist_hasher::hash_t> hashes_cont;

  hashes_stack(zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash, piece::color_t attacker,
               unsigned steps_remaining = MAX_STEPS) :
    stack(1, initial_hash),
    hasher(&hasher)
  { 
    last_visited.push(last(vertex::type_or, attacker, steps_remaining));
  }

  zobrist_hasher::hash_t top() const    { return stack.back(); }

  //! Get a container of the encountered hashes. The top of the stack is the last element in the container; bottom is the first.
  hashes_cont const&     hashes() const { return stack; }

  void push(vertex const& v);
  void pop();

private:
  struct last {
    vertex::e_type    type;
    piece::color_t    player;
    unsigned          steps_remaining;

    last(vertex::e_type t, piece::color_t p, unsigned s) :
      type(t),
      player(p),
      steps_remaining(s)
    { }
  };

  hashes_cont           stack;
  zobrist_hasher const* hasher;
  std::stack<last>      last_visited;
};

//! A meta visitor that keeps track of the current board state and runs another visitor on both the given vertex and the
//! computed board.
template <typename SubVisitor = null_board_visitor>
struct board_visitor {
  explicit board_visitor(apns::board const& initial_board, SubVisitor sub_visitor = SubVisitor()) :
    boards(initial_board),
    sub_visitor(sub_visitor)
  { }

  apns::board const& get_board() const {
    return boards.top();
  }

  board_stack& get_board_stack() {
    return boards;
  }

  void operator () (vertex& v) {
    if (v.step)
      boards.push(*v.step);
    boost::unwrap_ref(sub_visitor)(v, boards.top());
  }
  
private:
  board_stack boards;
  SubVisitor  sub_visitor;
};

//! Helper to make a board_visitor.
template <typename SubVisitor>
board_visitor<SubVisitor> make_board_visitor(apns::board const& initial_board, SubVisitor sub_visitor) {
  return board_visitor<SubVisitor>(initial_board, sub_visitor);
}

inline board_visitor<null_board_visitor> make_board_visitor(apns::board const& initial_board) {
  return board_visitor<null_board_visitor>(initial_board);
}

//! A visitor that allows composition of board_visitor sub-visitors. This is analogous to composite_visitor, except
//! it also passes the computed board to the sub-visitors.
template <typename First, typename Second>
struct board_composite_visitor {
  First   first;
  Second  second;

  explicit board_composite_visitor(First f = First(), Second s = Second()) :
    first(f),
    second(s)
  { }

  void operator () (vertex& v, board& b) {
    boost::unwrap_ref(first)(v, b);
    boost::unwrap_ref(second)(v, b);
  }
};

//! Visitor that keeps track of a game history. History is a sequence of pairs (board, player) -- each pair corresponds to a
//! game state at the beginning of a player's turn. This is a visitor for the board_visitor meta-visitor.
struct history_visitor {
  explicit history_visitor(piece::color_t attacker) : h(attacker) { }

  history_stack::records_cont const& get_history() {
    return h.records();
  }

  history_stack& get_history_stack() {
    return h;
  }

  void operator () (vertex const& v, board const& board) {
    h.push(v, board);
  }

private:
  history_stack h;
};

//! Visitor that keeps track of the hash of the board. This visitor keeps the complete history of hashes encountered during the
//! descent.
struct hash_visitor {
  //! Construct the visitor.
  //! \param hasher Hasher used for the algorithm.
  //! \param initial_hash Hash value of the initial board.
  //! \param attacker Attacker's colour.
  hash_visitor(zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash, piece::color_t attacker) :
    stack_(hasher, initial_hash, attacker)
  { }

  hashes_stack::hashes_cont const& hashes() const { return stack_.hashes(); }
  hashes_stack& stack()                           { return stack_; }
  void operator () (vertex const& v)              { stack_.push(v); }

private:
  hashes_stack stack_;
};

//! Visitor that keeps track of the visited vertices.
struct path_visitor {
  typedef std::vector<vertex*> path_cont;

  //! Vertices on the path root -> leaf, in the order they were encountered.
  path_cont path;

  void operator () (vertex& v) {
    path.push_back(&v);
  }
};

//! Killer steps database.
class killer_db {
  typedef boost::circular_buffer<step> ply_killers_t;

public:
  typedef ply_killers_t::const_iterator ply_iterator;

  //! Make a killer DB that holds at most killer_count killers for each ply.
  explicit killer_db(std::size_t const killer_count) : killer_count(killer_count) { }

  //! Add a step to the list of killers for given ply and given parent type.
  void add(std::size_t ply, vertex::e_type type, step const& step);

  //! Is the given step a killer for the given ply?
  bool is_killer(std::size_t ply, vertex::e_type type, step const& step);

  ply_iterator ply_begin(std::size_t ply, vertex::e_type type) const {
    if (ply < get_plys(type).size()) 
      return get_plys(type)[ply].begin();
    else
      return ply_iterator();
  }

  ply_iterator ply_end(std::size_t ply, vertex::e_type type) const {
    if (ply < get_plys(type).size())
      return get_plys(type)[ply].end();
    else
      return ply_iterator();
  };

private:
  typedef std::vector<ply_killers_t> plys;

  std::size_t const killer_count;       //!< How many killers at most per ply.
  plys killers_or;
  plys killers_and;

  plys& get_plys(vertex::e_type type) {
    switch (type) {
    case vertex::type_or:   return killers_or;
    case vertex::type_and:  return killers_and;
    }

    assert(!"Won't get here");
    return killers_or;
  }

  plys const& get_plys(vertex::e_type type) const {
    switch (type) {
    case vertex::type_or:   return killers_or;
    case vertex::type_and:  return killers_and;
    }

    assert(!"Wont' get here");
    return killers_or;
  }
};

//! Update proof- and disproof-numbers of a single vertex.
void update_numbers(vertex& v);

namespace detail {

  //! Expand a leaf.
  //!
  //! \param leaf The leaf to be expanded.
  //! \param state Board state of the leaf.
  //! \param attacker Attacker.
  //! \param trans_tbl Transposition table to use or NULL.
  //! \param hasher The hasher instance used for this tree.
  //! \param leaf_hash Hash value of leaf_state
  //! \param history Game history gathered during the descend to the leaf.
  void expand(vertex::children_iterator leaf, board_stack& state, piece::color_t attacker, transposition_table* trans_tbl,
              hashes_stack& hashes, history_stack::records_cont const& history);

  //! Process a newly vertex in the tree.
  void process_new(vertex& child, vertex const& parent, piece::color_t atatcker, step const& step, 
                   vertex::e_type type, transposition_table* trans_tbl, hashes_stack& hashes, board_stack& state,
                   history_stack::records_cont const& history);

  //! Simulate a subtree, if possible.
  //!
  //! \returns True if simulation has been successful, and parent has been proved (parent will have children attached, then).
  //! false otherwise.
  bool simulate(vertex& parent, killer_db& killers, std::size_t ply, piece::color_t attacker, board_stack& boards,
                hashes_stack& hashes, history_stack& history);

} // namespace apns::detail

//! A CRTP base class for search algorithms.
template <typename Algo>
class search_algo : private boost::noncopyable {
public:
  boost::shared_ptr<apns::game> get_game() const {
    return game;
  }

  bool finished() const {
    return game->root.proof_number == 0 || game->root.disproof_number == 0;
  }

  //! Make the algorithm use a transposition table.
  void use_trans_tbl(std::size_t size, std::size_t keep_time) {
    trans_tbl.reset(new transposition_table(size, keep_time)); 
  }

  //! Get the transposition table, if any, used by this algorithm.
  //! \returns Pointer to the transposition table or null if no table is associated with this algorithm.
  transposition_table const* get_trans_tbl() const {
    return trans_tbl.get(); 
  }

  //! Get the total number of vertices currently held by this algorithm.
  std::size_t get_position_count() const {
    return position_count;
  }

  //! Run the algorithm for ms_how_long milliseconds.
  void run(unsigned ms_how_long) {
    boost::timer timer;

    while (!finished() && (ms_how_long == 0 || timer.elapsed() < ms_how_long / 1000.0))
      iterate();
  }

  void iterate() {
    if (!finished()) {
      if (trans_tbl)
        trans_tbl->tick();
      static_cast<Algo*>(this)->do_iterate();
    }
  }

protected:
  static std::size_t const KILLERS_COUNT = 2;

  boost::shared_ptr<apns::game>  game;
  boost::scoped_ptr<transposition_table> trans_tbl;
  zobrist_hasher              hasher;         //!< Hasher to be used during the algorithm.
  zobrist_hasher::hash_t      initial_hash;   //!< Hash corresponding to initial_state.
  killer_db                   killers;
  std::size_t                 position_count;

  search_algo(boost::shared_ptr<apns::game> const& game, std::size_t position_count = 1) :
    game(game),
    initial_hash(hasher.generate_initial(game->initial_state, game->attacker)),
    killers(KILLERS_COUNT),
    position_count(position_count)
  { }

  template <typename PathIter>
  void process_leaf(PathIter path_begin, PathIter path_end,
                    board_stack& board_stack, hashes_stack& hashes_stack, history_stack& history) {
    if (path_begin != path_end) {
      std::size_t const leaf_ply = std::distance(path_begin, path_end) - 1;
      vertex::children_iterator leaf = *(path_end - 1);

      if (detail::simulate(*leaf, killers, leaf_ply, game->attacker, board_stack, hashes_stack, history)) {
        vertex_counter counter;
        traverse(*leaf, backtrack(), boost::ref(counter));
        position_count += counter.count;

      } else {
        expand(leaf, board_stack, hashes_stack, history);
        position_count += leaf->children_count();
      }

      update_numbers(std::reverse_iterator<PathIter>(path_end), std::reverse_iterator<PathIter>(path_begin),
                     hashes_stack.hashes().rbegin());

      if ((leaf->type == vertex::type_or && leaf->proof_number == 0)
          || (leaf->type == vertex::type_and && leaf->disproof_number == 0)) {
        vertex::number_t vertex::* num = leaf->type == vertex::type_or ? &vertex::proof_number : &vertex::disproof_number;
        vertex::children_iterator proof = std::find_if(leaf->children_begin(), leaf->children_end(),
                                                       boost::bind(num, _1) == 0);
        assert(proof != leaf->children_end());
        killers.add(leaf_ply + 1, leaf->type, *proof->step);
      }
    }
  }

  void expand(vertex::children_iterator leaf, board_stack& state, hashes_stack& hashes, history_stack const& history) {
    apns::detail::expand(leaf, state, game->attacker, trans_tbl.get(), hashes, history.records());
  }

  template <typename HashesRevIter, typename PathRevIter>
  void update_numbers(PathRevIter path_begin, PathRevIter path_end, HashesRevIter hashes_begin) {
    std::size_t ply = std::distance(path_begin, path_end) - 1;
    PathRevIter current = path_begin;

    while (current != path_end) {
      vertex& v = **current;
      apns::update_numbers(v);

      if (trans_tbl && v.proof_number != 0 && v.disproof_number != 0)
        trans_tbl->insert(*hashes_begin, std::make_pair(v.proof_number, v.disproof_number));

      if (current + 1 != path_end) {
        vertex& parent = **(current + 1);
        if ((parent.type == vertex::type_or && v.proof_number == 0)
            || (parent.type == vertex::type_and && v.disproof_number == 0))
          killers.add(ply, parent.type, *v.step);
      }

      ++current;
      ++hashes_begin;
      --ply;
    }
  }
};

//! The basic variant of the Proof-Number Search algorithm.
class proof_number_search : public search_algo<proof_number_search> {
public:
  //! Make an algorithm instance for the given game instance. 
  //!
  //! \param position_count Number of positions in the passed-in game.
  explicit proof_number_search(boost::shared_ptr<apns::game> const& game, std::size_t position_count = 1) :
    search_algo(game, position_count)
  { }

  //! Perform an iteration of the algorithm.
  void do_iterate();
};

//! Depth-first variant of the Proof-Number Search algorithm.
struct depth_first_pns : public search_algo<depth_first_pns> {
  explicit depth_first_pns(boost::shared_ptr<apns::game> const& game, std::size_t position_count = 1) :
    search_algo(game, position_count),
    path(1, &game->root),
    limits(1, limits_t(vertex::infty, vertex::infty)),
    history(game->attacker),
    hashes(hasher, initial_hash, game->attacker),
    boards(game->initial_state)
  { }

  void do_iterate();
  
private:
  struct limits_t {
    vertex::number_t pn_limit;
    vertex::number_t dn_limit;

    limits_t() { }
    limits_t(vertex::number_t pn, vertex::number_t dn) : pn_limit(pn), dn_limit(dn) { }
  };

  typedef std::vector<vertex*>  path_cont;
  typedef std::vector<limits_t> limits_cont;

  path_cont       path;
  limits_cont     limits;
  history_stack   history;
  hashes_stack    hashes;
  board_stack     boards;

  limits_t make_limits(vertex& v, vertex& parent, boost::optional<vertex&> second_best, limits_t parent_limits);
};

} // namespace apns

#endif

