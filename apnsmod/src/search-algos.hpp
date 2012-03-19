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

#include <vector>
#include <cassert>

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

//! A no-op visitor for board_visitor.
struct null_board_visitor {
  void operator () (vertex const&, board const&) { }
};

//! A meta visitor that keeps track of the current board state and runs another visitor on both the given vertex and the
//! computed board.
template <typename SubVisitor = null_board_visitor>
struct board_visitor {
  ::board     board;  //!< The computed board.
  SubVisitor  sub_visitor;

  explicit board_visitor(::board const& initial_board, SubVisitor sub_visitor = SubVisitor()) :
    board(initial_board),
    sub_visitor(sub_visitor)
  { }

  void operator () (vertex& v) {
    if (v.step)
      apply(*v.step, board);

    boost::unwrap_ref(sub_visitor)(v, board);
  }
};

//! Helper to make a board_visitor.
template <typename SubVisitor>
board_visitor<SubVisitor> make_board_visitor(::board const& initial_board, SubVisitor sub_visitor) {
  return board_visitor<SubVisitor>(initial_board, sub_visitor);
}

inline board_visitor<null_board_visitor> make_board_visitor(::board const& initial_board) {
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
  struct history_record {
    ::board         position;
    piece::color_t  player;

    history_record(::board const& b, piece::color_t p) :
      position(b),
      player(p)
    { }
  };

  typedef std::vector<history_record> history_cont;

  history_cont history;

  explicit history_visitor(piece::color_t attacker);
  bool operator () (vertex const& v, board const& board);

private:
  vertex::e_type  last_visited_type;
  piece::color_t  attacker;
};

//! Visitor that keeps track of the hash of the board. This visitor keeps the complete history of hashes encountered during the
//! descent.
struct hash_visitor {
  typedef std::vector<zobrist_hasher::hash_t> hashes_cont;
  hashes_cont hashes;

  //! Construct the visitor.
  //! \param hasher Hasher used for the algorithm.
  //! \param initial_hash Hash value of the initial board.
  //! \param attacker Attacker's colour.
  hash_visitor(zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash, piece::color_t attacker) :
    hashes(1, initial_hash),
    last_visited_type(vertex::type_or),
    last_visited_player(attacker),
    hasher(&hasher)
  { }

  bool operator () (vertex const& v);

private:
  vertex::e_type        last_visited_type;
  piece::color_t        last_visited_player;
  zobrist_hasher const* hasher;
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

//! Update proof- and disproof-numbers of a single vertex.
void update_numbers(vertex& v);

//! A CRTP base class for search algorithms.
template <typename Algo>
struct base_algo : boost::noncopyable {
  boost::shared_ptr< ::game> get_game() const {
    return static_cast<Algo const*>(this)->do_get_game();
  }

  bool finished() const {
    return static_cast<Algo const*>(this)->do_finished();
  }

  void iterate() {
    return static_cast<Algo*>(this)->do_iterate();
  }

  //! Run the algorithm for ms_how_long milliseconds.
  void run(unsigned ms_how_long) {
    boost::timer timer;

    while (!finished() && (ms_how_long == 0 || timer.elapsed() < ms_how_long / 1000.0))
      iterate();
  }

protected:
  base_algo() { }
};

//! The basic variant of the Proof-Number Search algorithm.
class proof_number_search : public base_algo<proof_number_search> {
public:
  //! Make an algorithm instance for the given game instance. 
  //!
  //! \param position_count Number of positions in the passed-in game.
  explicit proof_number_search(boost::shared_ptr< ::game> const& game, std::size_t position_count = 1) :
    game(game),
    initial_hash(hasher.generate_initial(game->initial_state, game->attacker)),
    position_count(position_count)
  { }

  //! Get the associated game instance.
  boost::shared_ptr< ::game> do_get_game() const {
    assert(game);
    return game; 
  }

  //! Has the algorithm finished?
  bool do_finished() const { 
    return game->root.proof_number == 0 || game->root.disproof_number == 0; 
  }

  //! Perform an iteration of the algorithm.
  void do_iterate();

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

private:
  boost::shared_ptr< ::game>  game;
  boost::scoped_ptr<transposition_table> trans_tbl;
  zobrist_hasher              hasher;         //!< Hasher to be used during the algorithm.
  zobrist_hasher::hash_t      initial_hash;   //!< Hash corresponding to initial_state.
  std::size_t                 position_count;

  //! Expand a leaf.
  //!
  //! \param leaf The leaf to be expanded.
  //! \param leaf_state Board state of the leaf.
  //! \param leaf_hash Hash value of leaf_state
  //! \param history Game history gathered during the descend to the leaf.
  void expand(vertex::children_iterator leaf, ::board& leaf_state,
              zobrist_hasher::hash_t leaf_hash, history_visitor::history_cont const& history);
};

#endif

