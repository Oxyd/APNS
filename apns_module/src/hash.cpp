#include "hash.hpp"
#include "board.hpp"
#include "util.hpp"

#include <boost/random/uniform_int.hpp>

#include <cstdlib>
#include <limits>

namespace {

typedef boost::uniform_int<zobrist_hasher::hash_t> zobrist_uniform_distrib_t;
typedef boost::multi_array_types::index_range range;

} // anonymous namespace

zobrist_hasher::zobrist_hasher() 
  : codes(boost::extents[piece::type_count][piece::color_count][board::ROWS][board::COLUMNS])
{
  hash_t const MAX_VALUE = std::numeric_limits<hash_t>::max();
  zobrist_uniform_distrib_t prng_distrib(0, MAX_VALUE);

  for (std::size_t type = 0; type < piece::type_count; ++type) {
    for (std::size_t color = 0; color < piece::color_count; ++color) {
      for (std::size_t row = board::MIN_ROW; row <= board::MAX_ROW; ++row) {
        for (std::size_t column = board::MIN_COLUMN; column <= board::MAX_COLUMN; ++column) {
          codes[type]
               [color]
               [row - board::MIN_ROW]
               [column - board::MIN_COLUMN] = prng_distrib(prng);
        }
      }
    }
  }

  for (std::size_t player = 0; player < piece::color_count; ++player) {
    players[player] = prng_distrib(prng);
  }
}

zobrist_hasher::hash_t zobrist_hasher::generate_initial(board const& board, piece::color_t on_move) const {
  hash_t hash = 0;

  for (board::pieces_iterator p = board.pieces_begin(); p != board.pieces_end(); ++p) {
    position const& position = p->first;
    piece const& piece = p->second;

    hash ^= codes[piece.get_type()]
                 [piece.get_color()]
                 [position.get_row() - board::MIN_ROW]
                 [position.get_column() - board::MIN_COLUMN];
  }

  hash ^= players[on_move];

  return hash;
}

zobrist_hasher::hash_t zobrist_hasher::update(hash_t old_hash,
    step::elementary_step_seq::const_iterator steps_begin, step::elementary_step_seq::const_iterator steps_end,
    piece::color_t current_player, piece::color_t next_player) const {
  hash_t hash = old_hash;

  for (step::elementary_step_seq::const_iterator step = steps_begin; step != steps_end; ++step) {
    position const& old_position = step->get_from();
    piece const& piece = *step->get_what();  // Assumed to be non-empty.

    // First remove the piece from its old position. If this is a displacement, add the piece's new position to the
    // hash later.
    hash ^= codes[piece.get_type()]
                 [piece.get_color()]
                 [old_position.get_row() - board::MIN_ROW]
                 [old_position.get_column() - board::MIN_COLUMN];

    if (!step->is_capture()) {
      position new_position = make_adjacent(old_position, step->get_where());
      hash ^= codes[piece.get_type()]
                   [piece.get_color()]
                   [new_position.get_row() - board::MIN_ROW]
                   [new_position.get_column() - board::MIN_COLUMN];
    }
  }

  hash ^= players[current_player];
  hash ^= players[next_player];

  return hash;
}

zobrist_hasher::hash_t zobrist_hasher::update(hash_t old_hash,
    step::elementary_step_seq const& steps,
    piece::color_t current_player, piece::color_t next_player) const {
  return update(old_hash, steps.begin(), steps.end(), current_player, next_player);
}
