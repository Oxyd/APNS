/**
 * \file board.hpp
 *
 * Types and functions related to the board itself and pieces standing on the 
 * board.
 */

#ifndef BOARD_HPP
#define BOARD_HPP

#include <boost/optional.hpp>
#include <boost/array.hpp>
#include <boost/iterator/iterator_adaptor.hpp>
#include <boost/cstdint.hpp>
#include <boost/static_assert.hpp>

#include <string>
#include <ostream>
#include <algorithm>
#include <algorithm>
#include <cstddef>

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

/// The four cardinal directions.
enum direction { north, south, east, west };

/// Return the letter representing the given direction according to the Arimaa
/// notation rules.
char letter_from_direction(direction d);

/**
 * Convert an \c int to #piece::color_t.
 * \throws std::domain_error if the value could not be converted.
 */
piece::color_t color_from_int(int value);

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
  position(row_t row, col_t column);
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

/**
 * Get a position adjacent to to the given one in the given direction.
 *
 * If there is no such position, throw std::logic_error.
 *
 * \param original Where from?
 * \param direction Where to?
 * \throws std::logic_error No such position exists in the Arimaa world.
 */
inline position make_adjacent(position original, direction direction) {
  int row = original.row();
  char column = original.column();

  switch (direction) {
    case north:   ++row;    break;
    case south:   --row;    break;
    case east:    ++column; break;
    case west:    --column; break;
    default:      assert(!"Can't reach this.");
  }

  return position(row, column);  // Will throw if the position is invalid.
}

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

/**
 * Convert an int to #piece::type_t.
 * \throws std::domain_error if the value could not be converted.
 */
piece::type_t type_from_int(int value);

/**
 * The board itself.
 *
 * Do note that board itself is large and copying may be expensive.
 */
class board {
public:
  // Dimensions of the board.
  static position::row_t const ROWS    = 8;
  static position::col_t const COLUMNS = 8;

  static position::row_t const MIN_ROW = 1;
  static position::row_t const MAX_ROW = 8;
  static position::col_t const MIN_COLUMN = 'a';
  static position::col_t const MAX_COLUMN = 'h';

  /// A mask of the board. This can act either as a filter or as partial
  /// information about the board.
  class mask {
    BOOST_STATIC_ASSERT(ROWS * COLUMNS == 64);

    // NB: I'm not using std::bitset here, because the de Bruijn multiplication
    // in .bitscan() wants to use integer arithmetic instructions on the bits.
    // std::bitset does have a .to_ulong member, however that only returns
    // unsigned long, which is not guaranteed to be 64 bits. In fact, it's
    // going to be 32 bits on common 32-bit systems, which I also want to
    // support.

    typedef boost::uint64_t bits_t;

  public:
    /// Forward iterator over the set positions of a mask.
    class iterator : public boost::iterator_facade<
      iterator,
      position,
      boost::forward_traversal_tag,
      position
    > {
    public:
      /// Constructs a singular iterator. This is equal to the end iterator of
      /// any mask.
      iterator() { }

      position dereference() const { return pos_ + offset_; }

      bool equal(iterator other) const {
        return mask_ == other.mask_;
      }

      void increment() {
        pos_ += offset_ + 1;
        mask_ >>= offset_ + 1;
        if (mask_ > 0)
          offset_ = bitscan();
      }

    private:
      friend class board::mask;

      bits_t      mask_;
      position    pos_;
      std::size_t offset_;

      explicit iterator(mask::bits_t m)
        : mask_(m)
        , pos_(position(MIN_ROW, MIN_COLUMN)) {
        if (mask_ > 0)
          offset_ = bitscan();
      }

      /// Get the index of least-significant set bit.
      std::size_t bitscan() const;
    };
    typedef iterator const_iterator;

    ///@{ Pre-defined masks
    static mask const ROW[];
    static mask const COLUMN[];
    static mask const TRAPS;
    ///@}

    ///@{ Constructors
    /// Create an empty mask.
    mask() : bits_(0) { }

    /// Create mask with true's on given row, and false's everywhere else.
    static mask row(position::row_t r) {
      return ROW[r - MIN_ROW];
    }

    /// Create mask with true's on given column and false everywhere else.
    static mask column(position::col_t c) {
      return COLUMN[c - MIN_COLUMN];
    }
    ///@}

    ///@{ Observers.
    bool get(position p) const  { return bits_ & (bits_t(1) << p.order()); }
    bool empty() const          { return bits_ == 0; }
    bool equals(mask other)     { return bits_ == other.bits_; }
    ///@}

    ///@{ Iteration
    iterator begin() const      { return iterator(bits_); }
    iterator end() const        { return iterator(); }
    ///@}

    ///@{ Modifiers
    void set(position p, bool value) {
      if (value)
        bits_ |= bits_t(1) << p.order();
      else
        bits_ &= ~(bits_t(1) << p.order());
    }

    /// Shift this mask in the given direction.
    mask shift(direction dir) const;
    ///@}

    ///@{ Operators
    mask operator ~ () { return mask(~bits_); }

    mask& operator &= (mask other) { bits_ &= other.bits_; return *this; }
    mask& operator |= (mask other) { bits_ |= other.bits_; return *this; }
    mask& operator ^= (mask other) { bits_ ^= other.bits_; return *this; }
    ///@}

  private:
    bits_t bits_;

    explicit mask(bits_t b) : bits_(b) { }
  };

private:
  /// Pieces are stored in a container of this type.
  typedef boost::array<
    char,
    ROWS * COLUMNS
  > pieces_cont;

public:
  /// Bidirectional iterator type over elements of type pair<position, piece>.
  class pieces_iterator : public boost::iterator_adaptor<
    pieces_iterator,
    pieces_cont::const_iterator,
    std::pair<position, piece> const,
    boost::bidirectional_traversal_tag,
    std::pair<position, piece> const
  >
  {
  public:
    pieces_iterator();  ///< Construct a singular iterator.

  private:
    /// Construct an iterator from an iterator into the underlying sequence
    /// given that the iterator into the underlying sequence corresponds with
    /// an element with an index pos.
    explicit pieces_iterator(base_type original, std::size_t pos);

    friend class boost::iterator_core_access;
    friend class board;

    reference dereference() const;  ///< Implement the * operation.
    void increment();               ///< Implement the ++ operation.
    void decrement();               ///< Implement the -- operation.

    /// Iterate to the first non-empty position at or after the current one.
    void forward_to_nonempty();

    /// Iterate to the first non-empty position at or before the current one.
    void reverse_to_nonempty();

    std::size_t pos_;  ///< Index into board::pieces that this iterator is
                       ///< currently pointing to.
  };

  board();

  /**
   * Put a piece on a given position.
   * \throws std::logic_error #where already contains a piece.
   */
  void put(position where, piece what);

  /**
   * Remove the piece from the given position.
   * \throws std::logic_error #where is empty.
   */
  void remove(position where);

  /**
   * Get a piece from the given position.
   *
   * \return The piece retreived from the position #from, or the empty value if 
   * there is no piece at that position.
   */
  boost::optional<piece> get(position from) const {
    char const p = pieces_[(from.row() - board::MIN_ROW) * board::COLUMNS +
                   (from.column() - board::MIN_COLUMN)];
    if (p != ' ')
      return piece_from_letter_unsafe(p);
    else
      return boost::none;
  }

  /// Remove all pieces from the board.
  void clear() {
    std::fill(pieces_.begin(), pieces_.end(), ' ');
  }

  pieces_iterator pieces_begin() const;  ///< Get the start iterator of the
                                         ///< sequence of all pieces stored
                                         ///< within this board.

  pieces_iterator pieces_end() const;    ///< Get the one-past-the-end iterator
                                         ///< of the sequence.

  /// Compare boards;
  bool equal(board const& other) const { return pieces_ == other.pieces_; }
  bool less(board const& other) const  { return pieces_ < other.pieces_; }

private:
  pieces_cont pieces_;
};

///@{ board::mask operators
inline bool operator == (board::mask lhs, board::mask rhs) {
  return lhs.equals(rhs);
}

inline bool operator != (board::mask lhs, board::mask rhs) {
  return !lhs.equals(rhs);
}

inline board::mask operator & (board::mask lhs, board::mask rhs) {
  return lhs &= rhs;
}

inline board::mask operator | (board::mask lhs, board::mask rhs) {
  return lhs |= rhs;
}

inline board::mask operator ^ (board::mask lhs, board::mask rhs) {
  return lhs ^= rhs;
}

/// Test whether two board contain exactly the same elements.
bool operator == (board const& lhs, board const& rhs);
bool operator != (board const& lhs, board const& rhs);
inline bool operator <  (board const& lhs, board const& rhs) {
  return lhs.less(rhs); 
}

/// Create a single-line string representation of a board.
std::string string_from_board(board const& board);

/// Re-create a board from a single-line string representation given by
/// string_from_board.
///
/// The input board will be .clear()ed before being populated with pieces.
///
/// \throws std::runtime_error Thrown if the string is not a valid
/// representation of a board.
void board_from_string(std::string const& string, apns::board& board);

/// Output a board object to an output string. This uses string_from_board to
/// format the board.
inline std::ostream& 
operator << (std::ostream& out, apns::board const& board) {
  return out << string_from_board(board);
}

/// Is the given position on a given board empty?
bool empty(position pos, board const& board);

/// Is the given position a trap?
bool trap(position pos);

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

/**
 * Iterator going through all pieces adjacent to a given one piece on a given 
 * board.
 */
class adjacent_pieces_iter : public boost::iterator_adaptor<
  adjacent_pieces_iter,
  neighbourhood_iter,
  piece,
  boost::forward_traversal_tag,
  piece
>
{
public:
  adjacent_pieces_iter();  ///< Construct a singular iterator.
  
  ///< Construct an iterator going around center on board.
  adjacent_pieces_iter(position center, apns::board const& board);  

private:
  friend class boost::iterator_core_access;

  void increment();
  reference dereference() const;
  bool equal(adjacent_pieces_iter const& other) const;

  /// Forward the underlying neighbourhood_iter to next nonempty position or
  /// keep it where it is, if it already points to a nonempty position.
  void forward_to_nonempty();  

  apns::board const* board_;
};

/// Return the start iterator of a sequence of pieces adjacent to the given
/// center position on the given board.
adjacent_pieces_iter
adjacent_pieces_begin(board const& board, position center);

/// Return the singular iterator of adjacent pieces sequence.
adjacent_pieces_iter adjacent_pieces_end();

// Why? Because I can. This is an often-used constructor, it might be 
// advantageous to give the compiler a better chance at inlining
// it.
inline position::position(row_t row, col_t column)
  : data_(((row - board::MIN_ROW) << ROW_OFFSET) | (column - board::MIN_COLUMN))
{
  if (row < board::MIN_ROW || row > board::MAX_ROW
      || column < board::MIN_COLUMN || column > board::MAX_COLUMN) {
    throw std::domain_error(
      "position::position: attempted to create an invalid position"
    );
  }
}

} // namespace apns

#endif

