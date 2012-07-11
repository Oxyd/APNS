#include "hash.hpp"
#include "board.hpp"
#include "movement.hpp"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <boost/bind.hpp>

namespace {

typedef boost::multi_array_types::index_range range;

} // anonymous namespace

apns::zobrist_hasher::zobrist_hasher() {
  boost::random::mt19937::result_type const SEED = 36992299;

  boost::random::mt19937 prng(SEED);
  boost::random::uniform_int_distribution<hash_t> rand_distrib;

  for (pieces_cont::iterator p = pieces_.begin(); p != pieces_.end(); ++p)
    *p = rand_distrib(prng);
  for (players_cont::iterator p = players_.begin(); p != players_.end(); ++p)
    *p = rand_distrib(prng);

  admits_double_ = rand_distrib(prng);
}

apns::zobrist_hasher::hash_t apns::zobrist_hasher::generate_initial(
  board const& board, piece::color_t on_move, int steps_remaining
) const {
  hash_t hash = 0;

  for (board::iterator p = board.begin(); p != board.end(); ++p) {
    position position = p->first;
    piece piece = p->second;

    hash ^= piece_code(piece, position);
  }

  hash ^= players_[index_from_color(on_move)];
  hash ^= steps_code(steps_remaining);

  return hash;
}

