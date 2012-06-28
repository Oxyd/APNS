#include "piece.hpp"
#include <boost/array.hpp>
#include <stdexcept>

namespace apns {

namespace {

char letter_from_pair(piece::color_t c, piece::type_t t) {
  char letter = t;
  if (c == piece::gold)
    letter &= ~0x20;  // Assuming ASCII here. Convert to uppercase.

  return letter;
}

}  // anonymous namespace

piece::piece(color_t c, type_t t) :
  data_(letter_from_pair(c, t))
{
  assert(data_ == 'e' || data_ == 'm' || data_ == 'h' || data_ == 'd' ||
         data_ == 'c' || data_ == 'r' || data_ == 'E' || data_ == 'M' ||
         data_ == 'H' || data_ == 'D' || data_ == 'C' || data_ == 'R');
}

piece::color_t piece::color() const {
  assert(data_ == 'e' || data_ == 'm' || data_ == 'h' || data_ == 'd' ||
         data_ == 'c' || data_ == 'r' || data_ == 'E' || data_ == 'M' ||
         data_ == 'H' || data_ == 'D' || data_ == 'C' || data_ == 'R');
  return data_ & 0x20 ? piece::silver : piece::gold;
}

piece::type_t piece::type() const {
  assert(data_ == 'e' || data_ == 'm' || data_ == 'h' || data_ == 'd' ||
         data_ == 'c' || data_ == 'r' || data_ == 'E' || data_ == 'M' ||
         data_ == 'H' || data_ == 'D' || data_ == 'C' || data_ == 'R');
  return static_cast<piece::type_t>(data_ | 0x20);
}

bool piece::equal(piece const& other) const {
  return data_ == other.data_;
}

boost::array<piece::color_t const, 2> const COLORS = { {
  piece::gold, piece::silver
} };

boost::array<piece::type_t const, 6> const TYPES = { {
  piece::elephant, piece::camel, piece::horse, piece::dog, piece::cat,
  piece::rabbit
} };

std::size_t index_from_color(piece::color_t color) {
  if (color == piece::gold)
    return 0;
  else
    return 1;
}

std::size_t index_from_type(piece::type_t type) {
  switch (type) {
  case piece::elephant:   return 0;
  case piece::camel:      return 1;
  case piece::horse:      return 2;
  case piece::dog:        return 3;
  case piece::cat:        return 4;
  case piece::rabbit:     return 5;
  default:                assert(!"Can't happen"); return 0;
  }
}

bool operator == (piece lhs, piece rhs) {
  return lhs.equal(rhs);
}

bool operator != (piece lhs, piece rhs) {
  return !operator == (lhs, rhs);
}

piece::color_t opponent_color(piece::color_t player) {
  switch (player) {
  case piece::gold:   return piece::silver;
  case piece::silver: return piece::gold;
  default:            assert(!"Doesn't get here."); break;
  }

  return piece::silver;  // Shut up, compiler.
}

char letter_from_piece(piece p) {
  return p.letter();
}

piece::color_t color_from_int(int value) {
  switch (value) {
  case piece::gold:   return piece::gold;
  case piece::silver: return piece::silver;
  default:
    throw std::domain_error(
      "Invalid integer representation of piece::color_t was given"
    );
  }
}

} // namespace apns
