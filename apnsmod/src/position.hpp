///
/// \file position.hpp Describing movement on the Arimaa board.
///

#ifndef APNS_POSITION_HPP
#define APNS_POSITION_HPP

#include <boost/iterator/iterator_facade.hpp>

#include <cstddef>
#include <stdexcept>
#include <string>

namespace apns {

/**
 * A position on the board.
 *
 * Position is a two-tuple in range (1, a) to (8, h). For convenience, one
 * invalid position is also supported: (9, a). This is to ease iteration over
 * all possible positions, mimicking the behaviour of iterators that also
 * support being set to a position one past the end.
 *
 * The one-past-the-end position may only be created through the default
 * constructor.
 */
class position {
  static unsigned char const  MAX         = 64;
  static std::size_t const    ROW_OFFSET  = 3;
  static unsigned char const  COLUMN_MASK = 0x7;

public:
  typedef unsigned      row_t;  ///< Type used for representation of rows.
  typedef unsigned char col_t;  ///< Type used for representation of columns.

  static row_t const MIN_ROW = 1;
  static row_t const MAX_ROW = 8;
  static col_t const MIN_COLUMN = 'a';
  static col_t const MAX_COLUMN = 'h';

  /// Construct the one-past-the-end position.
  position() : data_(MAX) { }

  /**
   * Construct a position.
   *
   * Both parameters are checked for valid value ranges,
   * (<tt>1 <= row <= 8, 'a' <= column <= 'h'</tt>). If one of the parameters
   * is invalid, throw std::invalid_argument.
   *
   * \param row The row.
   * \param column The column. This must be a lowercase letter, otherwise an
   *               exception will be thrown.
   * \throws std::invalid_argument Specified row or column was not valid.
   */
  position(row_t row, col_t column)
    : data_(((row - MIN_ROW) << ROW_OFFSET) | (column - MIN_COLUMN))
  {
    if (row < MIN_ROW || row > MAX_ROW
        || column < MIN_COLUMN || column > MAX_COLUMN) {
      throw std::domain_error(
        "position::position: attempted to create an invalid position"
      );
    }
  }

  position(row_t row, std::string const& column);

  ///@{ Observers
  row_t row() const;
  col_t column() const;
  std::string py_column() const;

  bool equals(position other) const { return data_ == other.data_; }

  /// True if this position preceeds the other in the order induced by op <.
  bool preceeds(position other) const { return data_ < other.data_; }

  /// Return integer n such that position(1, 'a') + n == *this.
  std::size_t order() const { return data_; }
  ///@}

  ///@{ Modifiers
  void set_row(row_t r);
  void set_column(col_t c);
  ///@}

  ///@{ Operators
  position& operator ++ () { return *this += 1; }
  position& operator -- () { return *this -= 1; }
  position  operator ++ (int) { position ret(*this); ++*this; return ret; }
  position  operator -- (int) { position ret(*this); --*this; return ret; }
  position& operator += (std::size_t n);
  position& operator -= (std::size_t n);
  ///@}

private:
  unsigned char data_;
};

///@{ position operators
inline bool operator == (position lhs, position rhs) {
  return lhs.equals(rhs);
}

inline bool operator != (position lhs, position rhs) {
  return !lhs.equals(rhs);
}

/**
 * Less-than compare two positions.
 *
 * This operator defines a linear order on all positions. This allows positions
 * to be stored in an ordered container such as std::set that requires that the
 * type be linearly ordered.
 *
 * This operator orders positions in row-major order, starting with row 1.
 */
inline bool operator < (position lhs, position rhs) {
  return lhs.preceeds(rhs);
}

/// Increase the position n times. p + n is equivalent to ++p; ++p; ... ++p;
/// n times.
/// \throw std::invalid_argument if this operation would result in an invalid
///        position.
inline position operator + (position p, std::size_t n) {
  return p += n;
}

inline position operator - (position p, std::size_t n) {
  return p -= n;
}

///@}

/// The four cardinal directions.
enum direction { north = 0, south, east, west };

/// Return the letter representing the given direction according to the Arimaa
/// notation rules.
char letter_from_direction(direction d);

/// Return the direction corresponding to the given letter.
direction direction_from_letter(char letter);

extern direction const directions[4];

/// Return an integer 0..3 representing the direction.
inline std::size_t index_from_direction(direction dir) { return static_cast<std::size_t>(dir); }

/**
 * Get a position adjacent to to the given one in the given direction.
 *
 * If there is no such position, throw std::logic_error.
 *
 * \param original Where from?
 * \param direction Where to?
 * \throws std::logic_error No such position exists in the Arimaa world.
 */
position make_adjacent(position original, direction direction);

/**
 * Is a position adjacent to the given one valid?
 *
 * An adjacent position is valid if it is inside the board bounds. That is, if
 * it is valid according to this function,
 * #make_adjacent is guaranteed not to throw while constructing it.
 *
 * \param original Where from?
 * \param dir Where to?
 */
bool adjacent_valid(position original, direction dir);

/// Are two positions adjacent to each other?
bool adjacent(position first, position second);

/// Get the direction in which #to is adjacent from #from. The two positions
/// must be adjacent.
/// \throw std::logic_error If !adjacent(from, to).
inline direction adjacent_dir(position from, position to) {
  if (from.row() == to.row()) {
    if (to.column() < from.column() && to.column() == from.column() - 1)
      return west;
    else if (to.column() > from.column() && to.column() == from.column() + 1)
      return east;
  } else {
    if (to.row() < from.row() && to.row() == from.row() - 1)
      return south;
    else if (to.row() > from.row() && to.row() == from.row() + 1)
      return north;
  }

  throw std::invalid_argument("adjacent_dir");
}

/// Iterator type going through all four possible directions.
typedef direction const* directions_iter;

directions_iter directions_begin();
directions_iter directions_end();

/**
 * This iterator goes over the neighbouring positions of a given central
 * position.
 */
class neighbourhood_iter : public boost::iterator_facade<
  neighbourhood_iter,
  position,
  boost::forward_traversal_tag,
  position
> {
public:
  neighbourhood_iter();                 ///< Construct a singular iterator.
  neighbourhood_iter(position center);  ///< Construct an iterator going around
                                        ///< in circle around center.

private:
  friend class boost::iterator_core_access;

  void increment();
  reference dereference() const;
  bool equal(neighbourhood_iter const& other) const;

  /// Forward direction to the first valid direction or keep it where it is, if
  /// it already points to a valid direction.
  void forward_to_valid();

  directions_iter direction_;
  position        center_;
};

neighbourhood_iter neighbourhood_begin(position center);
neighbourhood_iter neighbourhood_end();

} // namespace apns

#endif
