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

  std::generate(pieces_.begin(), pieces_.end(), boost::bind(rand_distrib, prng));
  std::generate(players_.begin(), players_.end(), boost::bind(rand_distrib, prng));
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

  hash ^= players_[on_move];

  if (steps_remaining >= 2)
    hash ^= admits_double_;

  return hash;
}

