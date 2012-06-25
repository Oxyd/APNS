/**
 * \file board.hpp
 *
 * Types and functions related to the board itself and pieces standing on the 
 * board.
 */

#ifndef BOARD_HPP
#define BOARD_HPP

#include "piece.hpp"
#include "position.hpp"

#include <boost/optional.hpp>
#include <boost/array.hpp>
#include <boost/iterator/iterator_adaptor.hpp>
#include <boost/cstdint.hpp>
#include <boost/static_assert.hpp>
#include <boost/ref.hpp>

#include <string>
#include <ostream>
#include <algorithm>
#include <algorithm>
#include <cstddef>

namespace apns {

/**
 * The board itself.
 */
class board {
public:
  // Dimensions of the board.
  static position::row_t const ROWS    = 8;
  static position::col_t const COLUMNS = 8;

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

    // Safe bool idiom.
    typedef void (mask::* bool_type)() const;
    void true_() const { }

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
      iterator() : mask_(0), offset_(0) { }

    private:
      friend class board::mask;
      friend class boost::iterator_core_access;

      bits_t      mask_;
      position    pos_;
      std::size_t offset_;

      explicit iterator(mask::bits_t m);

      position  dereference() const         { return pos_ + offset_; }
      bool      equal(iterator other) const { return mask_ == other.mask_; }
      void      increment();
    };
    typedef iterator const_iterator;

    /// References a single bit in the mask.
    class reference {
    public:
      operator bool () const { return bits_ & (bits_t(1) << order_); }
      reference& operator = (bool b) {
        if (b)
          bits_ |= (bits_t(1) << order_);
        else
          bits_ &= ~(bits_t(1) << order_);

        return *this;
      }

    private:
      friend class mask;

      bits_t&     bits_;
      std::size_t order_;

      reference(bits_t& b, std::size_t order) : bits_(b), order_(order) { }
    };

    /// \name Pre-defined masks
    ///@{
    static mask const ROW[];
    static mask const COLUMN[];
    static mask const TRAPS;
    ///@}

    /// \name Constructors
    ///@{

    /// Create an empty mask.
    mask() : bits_(0) { }

    /// Create a mask with one bit set.
    explicit mask(position p) : bits_(bits_t(1) << p.order()) { }

    /// Create mask with true's on given row, and false's everywhere else.
    static mask row(position::row_t r) {
      return ROW[r - position::MIN_ROW];
    }

    /// Create mask with true's on given column and false everywhere else.
    static mask column(position::col_t c) {
      return COLUMN[c - position::MIN_COLUMN];
    }

    ///@}

    /// \name Observers
    ///@{
    bool get(position p) const  { return bits_ & (bits_t(1) << p.order()); }
    bool empty() const          { return bits_ == 0; }
    bool equals(mask other)     { return bits_ == other.bits_; }
    bool less(mask other)       { return bits_ < other.bits_; }

    /// Lowest position (as defined by op < (position, position)) that is set.
    /// \throw std::logic_error If .empty().
    position first_set() const;
    ///@}

    /// \name Conversion
    ///@{

    /// A mask evaluates to true iff it is not .empty().
    operator bool_type () const { return !empty() ? &mask::true_ : 0; }

    ///@}

    /// \name Iteration
    ///@{
    iterator begin() const      { return iterator(bits_); }
    iterator end() const        { return iterator(); }
    ///@}

    /// \name Modifiers
    ///@{
    void set(position p, bool value) {
      if (value)
        bits_ |= bits_t(1) << p.order();
      else
        bits_ &= ~(bits_t(1) << p.order());
    }

    /// Shift this mask in the given direction.
    mask shift(direction dir) const;
    ///@}

    /// Operators
    ///@{
    reference operator [] (position p) { return reference(bits_, p.order()); }
    bool operator [] (position p) const { return get(p); }
    mask operator ~ () const { return mask(~bits_); }

    mask& operator &= (mask other) { bits_ &= other.bits_; return *this; }
    mask& operator |= (mask other) { bits_ |= other.bits_; return *this; }
    mask& operator ^= (mask other) { bits_ ^= other.bits_; return *this; }
    ///@}

  private:
    bits_t bits_;

    explicit mask(bits_t b) : bits_(b) { }
  };

private:
  typedef boost::array<mask, colors_array_t::static_size> players_masks;
  typedef boost::array<mask, types_array_t::static_size>  types_masks;

public:
  /// Forward iterator type over elements of type pair<position, piece>.
  class iterator : public boost::iterator_adaptor<
    iterator,
    mask::const_iterator,
    std::pair<position, piece> const,
    boost::forward_traversal_tag,
    std::pair<position, piece> const
  > {
  public:
    /// Construct a singular iterator.
    iterator()
      : iterator::iterator_adaptor_(mask::const_iterator())
      , board_(0) { }

  private:
    friend class boost::iterator_core_access;
    friend class board;

    board const*  board_;

    iterator(board const& board, mask::const_iterator b);
    std::pair<position, piece>  dereference() const;
  };

  board() { clear(); }

  /// \name Modifiers
  ///@{

  /// Put a piece on a given position.
  /// \throws std::logic_error #where already contains a piece.
  void put(position where, piece what);

  /// Remove the piece from the given position.
  /// \throws std::logic_error #where is empty.
  void remove(position where);

  /// Remove all pieces from the board.
  void clear();
  ///@}

  /// \name Observers
  ///@{

  /// Get a piece from the given position.
  ///
  /// \return The piece retreived from the position #from, or the empty value if
  /// there is no piece at that position.
  boost::optional<piece> get(position from) const;

  /// Mask of positions that are occupied.
  mask                  occupied() const  { return occupied_; }

  /// A mask for each player with positions occupied by the respective player.
  players_masks const&  players() const   { return players_; }

  /// A mask for each type with positions occupied by the respective type.
  types_masks const&    types() const     { return types_; }

  ///@}

  /// \name Comparison
  ///@{
  bool equal(board const& other) const;
  bool less(board const& other) const;
  ///@}

  /// \name Iteration
  ///@{
  iterator begin() const;
  iterator end() const;
  ///@}

private:
  players_masks players_;
  types_masks   types_;
  mask          occupied_;
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

inline bool operator < (board::mask lhs, board::mask rhs) {
  return lhs.less(rhs);
}

/// Test whether two board contain exactly the same elements.
inline bool operator == (board const& lhs, board const& rhs) {
  return lhs.equal(rhs);
}

inline bool operator != (board const& lhs, board const& rhs) {
  return !(lhs == rhs);
}

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

/// Bitmask representation of a board.
struct board_masks {
  typedef boost::array<board::mask, colors_array_t::static_size>
    player_masks_cont;
  typedef boost::array<board::mask, types_array_t::static_size>
    type_masks_cont;

  board::mask       occupied;
  player_masks_cont players;
  type_masks_cont   types;

};

inline bool operator == (board_masks const& lhs, board_masks const& rhs) {
  return
    lhs.occupied == rhs.occupied &&
    lhs.players == rhs.players &&
    lhs.types == rhs.types;
}

/// Convert board to bitmasks.
board_masks masks_from_board(board const& b);

/// Get a mask representing the valid neighbourhood of pos.
board::mask neighbourhood(position pos);

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

} // namespace apns

#endif

