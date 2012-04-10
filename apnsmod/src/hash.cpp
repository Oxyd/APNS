#include "hash.hpp"
#include "board.hpp"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>

namespace {

typedef boost::multi_array_types::index_range range;

} // anonymous namespace

apns::zobrist_hasher::zobrist_hasher() 
  : codes(boost::extents[piece::type_count][piece::color_count][board::ROWS][board::COLUMNS])
{
  boost::random::mt19937 prng;
  boost::random::uniform_int_distribution<hash_t> rand_distrib;

  for (types_array_t::const_iterator type = TYPES.begin(); type != TYPES.end(); ++type)
    for (colors_array_t::const_iterator color = COLORS.begin(); color != COLORS.end(); ++color)
      for (std::size_t row = board::MIN_ROW; row <= board::MAX_ROW; ++row)
        for (std::size_t column = board::MIN_COLUMN; column <= board::MAX_COLUMN; ++column)
          codes[type - TYPES.begin()]
               [color - COLORS.begin()]
               [row - board::MIN_ROW]
               [column - board::MIN_COLUMN] = rand_distrib(prng);

  for (std::size_t player = 0; player < piece::color_count; ++player)
    players[player] = rand_distrib(prng);
}

apns::zobrist_hasher::hash_t apns::zobrist_hasher::generate_initial(board const& board, piece::color_t on_move) const {
  hash_t hash = 0;

  for (board::pieces_iterator p = board.pieces_begin(); p != board.pieces_end(); ++p) {
    position position = p->first;
    piece piece = p->second;

    hash ^= codes[index_from_type(piece.get_type())]
                 [index_from_color(piece.get_color())]
                 [position.get_row() - board::MIN_ROW]
                 [position.get_column() - board::MIN_COLUMN];
  }

  hash ^= players[on_move];

  return hash;
}

