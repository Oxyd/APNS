#include "hash.hpp"
#include "board.hpp"
#include "movement.hpp"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <boost/bind.hpp>

#include <vector>

namespace apns {

namespace {

typedef boost::multi_array_types::index_range range;

template <typename Iter, typename Distrib, typename Prng>
void rand_fill(Iter begin, Iter end, Distrib distr, Prng& prng,
               std::vector<typename std::iterator_traits<Iter>::value_type>& used) {
  typedef typename std::iterator_traits<Iter>::value_type value_type;

  while (begin != end) {
    value_type v;
    do
      v = distr(prng);
    while (std::find(used.begin(), used.end(), v) != used.end());

    *begin++ = v;
  }
}

} // anonymous namespace

zobrist_hasher::zobrist_hasher() {
  boost::random::mt19937::result_type const SEED = 36992299;

  boost::random::mt19937 prng(SEED);
  boost::random::uniform_int_distribution<hash_t> rand_distrib;

  std::vector<hash_t> used_values;

  rand_fill(pieces_.begin(), pieces_.end(), rand_distrib, prng, used_values);
  rand_fill(players_.begin(), players_.end(), rand_distrib, prng, used_values);
  rand_fill(steps_.begin(), steps_.end(), rand_distrib, prng, used_values);
}

zobrist_hasher::hash_t zobrist_hasher::generate_initial(
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

}

