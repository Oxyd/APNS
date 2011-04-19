/**
 * \file board.hpp
 *
 * Types and functions related to the board itself and pieces standing on the board.
 */

#ifndef BOARD_HPP
#define BOARD_HPP

#include <boost/optional.hpp>
#include <boost/array.hpp>
#include <boost/function.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/filter_iterator.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/python.hpp>

#include <map>
#include <list>
#include <string>

/**
 * One game piece.
 *
 * A piece is immutable. Once it has been created, its attributes cannot be changed. It can, however, be assigned.
 */
struct piece {
  //! Color of the piece.
  enum color_t {
    gold = 0,
    silver,

    color_count = 2  //!< Number of colors total.
  };

  //! Type of the piece. That is, the animal that's used to represent it.
  enum type_t {
    elephant = 0,
    camel,
    horse,
    dog,
    cat,
    rabbit,

    type_count = 6  //!< How many animals are there in total in our zoo?
  };

  //! Init a piece with given color and type.
  piece(color_t color = gold, type_t type = elephant);

  color_t get_color() const;  //!< Get the color of this piece.
  type_t  get_type() const;   //!< Get the type of this piece.

private:
  color_t color;
  type_t  type;
};

//! Equality comparison of pieces.
bool operator == (piece lhs, piece rhs);

//! Returns the negation of operator ==.
bool operator != (piece lhs, piece rhs);

//! Return the letter representing given piece according to the Arimaa notation rules.
char piece_letter(piece p);

//! The four cardinal directions.
enum direction { north, south, east, west };

//! Return the letter representing the given direction according to the Arimaa notation rules.
char direction_letter(direction d);

/**
 * Convert an \c int to #piece::color_t.
 * \throws std::domain_error if the value could not be converted.
 */
piece::color_t color_from_int(int value);

/**
 * A position on the board.
 *
 * Position is immutable but it's copyable and assignable.
 */
class position {
private:
  /**
   * Python-specific typedef.
   *
   * <tt>unsigned char</tt> is an integral type, which results in an awkward syntax from the Python side, as Python doesn't have
   * any 'single character' type. So we'll provide overloads where column is represented by a single-character string. This will
   * provide nice syntax from the Python side, namely something like Position(1, 'c').
   */
  typedef std::string   python_col_t;

public:
  typedef unsigned      row_t;  //!< Type used for representation of rows.
  typedef unsigned char col_t;  //!< Type used for representation of columns.

  /**
   * Construct a position.
   *
   * Both parameters are checked for valid value ranges, (<tt>1 <= row <= 8, 'a' <= column <= 'h'</tt>). If one
   * of the parameters is invalid, throw std::domain_error.
   *
   * \param row The row.
   * \param column The column. This must be a lowercase letter, otherwise an exception will be thrown.
   * \throws std::domain_error Specified row or column was not valid.
   */
  position(row_t row, col_t column);

  /**
   * You don't want to use this.
   *
   * It's intended for use from Python only to allow construction using a single-letter string for column, instead
   * of just a char. It's not meant to be used from C++ code. Making it private, though, seems to cause more trouble
   * than it's worth. Namely, this type would have to befriend some private helper type from deep within Boost.Python
   * bowels.
   */
  position(row_t row, python_col_t const& column);

  row_t get_row() const;
  col_t get_column() const;

private:
  row_t row;
  col_t column;

  //! For Python bindings only.
  python_col_t get_column_py() const;

  //! Allow access to <tt>get_column_py</tt>. This function is defined in py_iface.cpp.
  friend void export_board();
};

bool operator == (position lhs, position rhs);  //!< Compare two positions for equality.
bool operator != (position lhs, position rhs);  //!< Compare two positions for inequality.

/**
 * Less-than compare two positions.
 *
 * This operator defines a linear order on all positions. This allows positions to be stored in an ordered container
 * such as std::set that requires that the type be linearly ordered.
 *
 * The exact ordering defined by this operator is further unspecified and should not be relied upon.
 */
bool operator < (position lhs, position rhs);

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
 * An adjacent position is valid if it is inside the board bounds. That is, if it is valid according to this function,
 * #make_adjacent is guaranteed not to throw while constructing it.
 *
 * \param original Where from?
 * \param dir Where to?
 */
bool adjacent_valid(position original, direction dir);

//! Are two positions adjacent to each other?
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
  static position::row_t const ROWS    = 8;         //!< How many rows a board has.
  static position::col_t const COLUMNS = 8;         //!< How many columns a board has.

  static position::row_t const MIN_ROW = 1;         //!< Coordinate of the first row.
  static position::row_t const MAX_ROW = 8;         //!< Coordinate of the last row.
  static position::col_t const MIN_COLUMN = 'a';    //!< Coordinate of the first column.
  static position::col_t const MAX_COLUMN = 'h';    //!< Coordinate of the last column.

private:
  //! Pieces are stored in a container of this type.
  typedef boost::array<
    boost::optional<piece>,
    ROWS * COLUMNS
  > pieces_cont;

public:
  //! Bidirectional iterator type over elements of type pair<position, piece>.
  class pieces_iterator : public boost::iterator_adaptor<
    pieces_iterator,
    pieces_cont::const_iterator,
    std::pair<position, piece> const,
    boost::bidirectional_traversal_tag,
    std::pair<position, piece> const
  >
  {
  public:
    pieces_iterator();  //!< Construct a singular iterator.

  private:
    //! Construct an iterator from an iterator into the underlying sequence given that the iterator into the
    //! underlying sequence corresponds with an element with an index pos.
    explicit pieces_iterator(base_type original, std::size_t pos);

    friend class boost::iterator_core_access;
    friend class board;

    reference dereference() const;  //!< Implement the * operation.
    void increment();               //!< Implement the ++ operation.
    void decrement();               //!< Implement the -- operation.

    //! Iterate to the first non-empty position at or after the current one.
    void forward_to_nonempty();

    //! Iterate to the first non-empty position at or before the current one.
    void backward_to_nonempty();

    std::size_t pos;  //!< Index into board::pieces that this iterator is currently pointing to.
  };

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
   * \return The piece retreived from the position #from, or the empty value if there is no piece at that position.
   */
  boost::optional<piece> get(position from) const;

  pieces_iterator pieces_begin() const;  //!< Get the start iterator of the sequence of all pieces stored within board.
  pieces_iterator pieces_end() const;    //!< Get the one-past-the-end iterator of the sequence.

private:
  pieces_cont       pieces;
};

bool operator == (board const& lhs, board const& rhs);  //<! Test whether two board contain exactly the same elements.
bool operator != (board const& lhs, board const& rhs);

//! Create a single-line string representation of a board.
std::string string_from_board(board const& board);

//! Is the given position on a given board empty?
bool empty(position pos, board const& board);

//! Is the given position a trap?
bool trap(position pos);

//! Iterator type going through all four possible directions.
typedef direction const* directions_iter;

directions_iter directions_begin();  //!< Get the beginning of the sequence of directions.
directions_iter directions_end();    //!< Get the end of the sequence of directions.

/**
 * This iterator goes over the neighbouring positions of a given central position.
 */
class neighbourhood_iter : public boost::iterator_facade<
  neighbourhood_iter,
  position,
  boost::forward_traversal_tag,
  position
> {
public:
  neighbourhood_iter();                 //!< Construct a singular iterator.
  neighbourhood_iter(position center);  //! Construct an iterator going around in circle around center.

private:
  friend class boost::iterator_core_access;

  void increment();
  reference dereference() const;
  bool equal(neighbourhood_iter const& other) const;

  void forward_to_valid();  //!< Forward direction to the first valid direction or keep it where it is, if it already
                            //!< points to a valid direction.

  directions_iter direction;
  position center;
};

neighbourhood_iter neighbourhood_begin(position center);  //!< Get the beginning of a position's neighbourhood.
neighbourhood_iter neighbourhood_end();                   //!< Get the end of a neighbourhood.

/**
 * Iterator going through all pieces adjacent to a given one piece on a given board.
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
  adjacent_pieces_iter();  //!< Construct a singular iterator.
  adjacent_pieces_iter(position center, ::board const& board);  //!< Construct an iterator going around center on board.

private:
  friend class boost::iterator_core_access;

  void increment();
  reference dereference() const;
  bool equal(adjacent_pieces_iter const& other) const;

  void forward_to_nonempty();  //!< Forward the underlying neighbourhood_iter to next nonempty position or keep it
                               //!< where it is, if it already points to a nonempty position.

  ::board const* board;
};

//! Return the start iterator of a sequence of pieces adjacent to the given center position on the given board.
adjacent_pieces_iter adjacent_pieces_begin(board const& board, position center);

//! Return the singular iterator of adjacent pieces sequence.
adjacent_pieces_iter adjacent_pieces_end();

#endif

