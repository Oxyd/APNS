#ifndef SEARCH_ALGOS_HPP
#define SEARCH_ALGOS_HPP

#include "movement.hpp"
#include "tree.hpp"

#include <vector>

#include <boost/shared_ptr.hpp>
#include <boost/utility.hpp>
#include <boost/timer.hpp>

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

    sub_visitor(v, board);
  }
};

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
    first(v, b);
    second(v, b);
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
  void operator () (vertex const& v, board const& board);

private:
  vertex::e_type  last_visited_type;
  piece::color_t  attacker;
};

//! Visitor that keeps track of the hash of a board.
struct hash_visitor {
  zobrist_hasher::hash_t  hash;

  //! Construct the visitor.
  //! \param hasher Hasher used for the algorithm.
  //! \param initial_hash Hash value of the initial board.
  //! \param attacker Attacker's colour.
  hash_visitor(zobrist_hasher const& hasher, zobrist_hasher::hash_t initial_hash, piece::color_t attacker) :
    hash(initial_hash),
    last_visited_type(vertex::type_or),
    last_visited_player(attacker),
    hasher(&hasher)
  { }

  void operator () (vertex const& v);

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
  //! Make an algorithm instance for the given game instance. This algorithm takes shared ownership of the game instance.
  explicit proof_number_search(boost::shared_ptr< ::game> const& game) :
    initial_hash(hasher.generate_initial(game->initial_state, game->attacker))
  { }

  //! Get the associated game instance.
  boost::shared_ptr< ::game> do_get_game() const { return game; }

  //! Has the algorithm finished?
  bool do_finished() const { return game->root.proof_number == 0 || game->root.disproof_number == 0; }

  //! Perform an iteration of the algorithm.
  void do_iterate();

private:
  //! Helper wrapper class for the visitors used during this algorithm.
  struct leaf_visitors {
    typedef composite_visitor<
      hash_visitor,
      composite_visitor<
        path_visitor,
        board_visitor<
          history_visitor
        >
      >
    > visitor;

    visitor v;

    leaf_visitors(::board const& initial_board, zobrist_hasher const& hasher,
                  zobrist_hasher::hash_t initial_hash, piece::color_t attacker) :
      v(
        hash_visitor(hasher, initial_hash, attacker),
        composite_visitor<path_visitor, board_visitor<history_visitor> >(
          path_visitor(),
          board_visitor<history_visitor>(
            initial_board,
            history_visitor(
              attacker))))
    { }

    void operator () (vertex& vertex) {
      v(vertex);
    }

    zobrist_hasher::hash_t get_hash()             { return v.first.hash; } 
    path_visitor::path_cont& get_path()           { return v.second.first.path; }
    ::board& get_board()                          { return v.second.second.board; }
    history_visitor::history_cont& get_history()  { return v.second.second.sub_visitor.history; }
  };

  boost::shared_ptr< ::game>  game;
  zobrist_hasher              hasher;         //!< Hasher to be used during the algorithm.
  zobrist_hasher::hash_t      initial_hash;   //!< Hash corresponding to initial_state.

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

