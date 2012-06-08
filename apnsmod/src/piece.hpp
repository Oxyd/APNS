///
/// \file piece.hpp: Arimaa piece representation
///

#ifndef APNS_PIECE_HPP
#define APNS_PIECE_HPP

#include <boost/array.hpp>
#include <boost/optional.hpp>

namespace apns {

/**
 * One game piece.
 *
 * A piece is immutable. Once it has been created, its attributes cannot be
 * changed. It can, however, be assigned.
 */
struct piece {
  /// Color of the piece.
  enum color_t {
    gold = 0,
    silver,

    color_count = 2  ///< Number of colors total.
  };

  /// Type of the piece. That is, the animal that's used to represent it.
  enum type_t {
    elephant  = 'e',
    camel     = 'm',
    horse     = 'h',
    dog       = 'd',
    cat       = 'c',
    rabbit    = 'r',

    type_count = 6  ///< How many animals are there in total in our zoo?
  };

  /// Init a piece with given color and type.
  piece(color_t color, type_t type);

  color_t color() const;  ///< Get the color of this piece.
  type_t  type() const;   ///< Get the type of this piece.

  /// Compare two pieces.
  bool equal(piece const& other) const;

  /// Get the Arimaa letter for this piece.
  char letter() const { return data_; }

private:
  friend piece piece_from_letter_unsafe(char);

  char data_;

  explicit piece(char letter) : data_(letter) {
    assert(data_ == 'e' || data_ == 'm' || data_ == 'h' || data_ == 'd' ||
           data_ == 'c' || data_ == 'r' || data_ == 'E' || data_ == 'M' ||
           data_ == 'H' || data_ == 'D' || data_ == 'C' || data_ == 'R');
  }
};

typedef boost::array<piece::color_t const, 2> colors_array_t;
typedef boost::array<piece::type_t const, 6> types_array_t;

extern colors_array_t const COLORS;  ///< Array of all colours.
extern types_array_t const TYPES;    ///< Array of all types. This array is
                                     ///< sorted from the strongest type to the
                                     ///< weakest.

/// Convert color to its index in COLORS.
std::size_t index_from_color(piece::color_t color);

///< Convert type to its index in TYPES.
std::size_t index_from_type(piece::type_t type);

/// Equality comparison of pieces.
bool operator == (piece lhs, piece rhs);

/// Returns the negation of operator ==.
bool operator != (piece lhs, piece rhs);

/// Return the opponent's color if the player's one is 'player'.
piece::color_t opponent_color(piece::color_t player);

/// Return the letter representing given piece according to the Arimaa notation
/// rules.
char letter_from_piece(piece p);

/// Same as piece_from_letter but assumes that the input is valid.
inline piece piece_from_letter_unsafe(char letter) {
  return piece(letter);
}

/// Convert a piece letter into a piece, or nothing if the letter doesn't
/// describe a valid piece.
inline boost::optional<piece> piece_from_letter(char letter) {
  if (letter == 'e' || letter == 'm' || letter == 'h' || letter == 'd' ||
      letter == 'c' || letter == 'r' || letter == 'E' || letter == 'M' ||
      letter == 'H' || letter == 'D' || letter == 'C' || letter == 'R')
    return piece_from_letter_unsafe(letter);
  else
    return boost::none;
}

/**
 * Convert an \c int to #piece::color_t.
 * \throws std::domain_error if the value could not be converted.
 */
piece::color_t color_from_int(int value);

} // namespace apns

#endif
