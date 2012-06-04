#include "hash.hpp"
#include "board.hpp"
#include "movement.hpp"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>

namespace {

typedef boost::multi_array_types::index_range range;

} // anonymous namespace

apns::zobrist_hasher::zobrist_hasher() 
  : codes_(boost::extents[piece::type_count][piece::color_count]
                         [board::ROWS][board::COLUMNS])
{
  boost::random::mt19937::result_type const SEED = 36992299;

  boost::random::mt19937 prng(SEED);
  boost::random::uniform_int_distribution<hash_t> rand_distrib;

  for (types_array_t::const_iterator type = TYPES.begin(); type != TYPES.end(); 
       ++type)
    for (colors_array_t::const_iterator color = COLORS.begin();
         color != COLORS.end(); ++color)
      for (
        std::size_t row = position::MIN_ROW; row <= position::MAX_ROW; ++row
      )
        for (
          std::size_t column = position::MIN_COLUMN;
          column <= position::MAX_COLUMN;
          ++column
        )
          codes_[type - TYPES.begin()]
                [color - COLORS.begin()]
                [row - position::MIN_ROW]
                [column - position::MIN_COLUMN] = rand_distrib(prng);

  for (std::size_t player = 0; player < piece::color_count; ++player)
    players_[player] = rand_distrib(prng);

  admits_double_ = rand_distrib(prng);
}

apns::zobrist_hasher::hash_t apns::zobrist_hasher::generate_initial(
  board const& board, piece::color_t on_move, int steps_remaining
) const {
  hash_t hash = 0;

  for (board::iterator p = board.begin();
       p != board.end(); ++p) {
    position position = p->first;
    piece piece = p->second;

    hash ^= codes_[index_from_type(piece.type())]
                  [index_from_color(piece.color())]
                  [position.row() - position::MIN_ROW]
                  [position.column() - position::MIN_COLUMN];
  }

  hash ^= players_[on_move];

  if (steps_remaining >= 2)
    hash ^= admits_double_;

  return hash;
}

