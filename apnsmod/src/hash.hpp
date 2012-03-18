/**
 * \file hash.hpp
 *
 * Hashing algorithms and the transposition table.
 */

#ifndef HASH_HPP
#define HASH_HPP

#include "board.hpp"
#include "movement.hpp"

#include <boost/array.hpp>
#include <boost/multi_array.hpp>
#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/utility.hpp>

#include <iostream>

#ifdef BOOST_NO_INT64_T
// Defined only if the underlying platform doesn't provide 64-bit integer types. This program can't work without
// 64-bit integer types.
# error "Your platform does not support 64-bit integer types. Sorry."
#endif

/**
 * Zobrist's algorithm to generate hashes for game positions. This hasher is copyable and assignable, but neither
 * copying nor assigning is cheap.
 */
class zobrist_hasher {
public:
  typedef boost::uint64_t hash_t;

  /**
   * Initialise the hasher. This constructor will generate 770 random numbers and store them in memory, so the construction
   * is heavy. It uses the pseudo-random number generator defined in \c util.hpp.
   */
  zobrist_hasher();

  /**
   * Generate the initial hash for the given board. This is meant to be only called once and then updated through
   * the update function.
   *
   * \param board The board for which the hash will be calculated.
   * \param on_move Whose turn is it?
   * \return The hash value.
   */
  hash_t generate_initial(board const& board, piece::color_t on_move) const;

  /**
   * Update the hash value after the situation on board has changed.
   *
   * \param old_hash Previous value of the hash.
   * \param steps_begin Beginning of a sequence of elementary steps that describe the movement of the pieces on the board.
   * \param steps_end End of the sequence of elementary steps.
   * \param current_player Who made the steps described?
   * \param next_player Whose turn is it now?
   */
  hash_t update(hash_t old_hash,
      step::elementary_step_seq::const_iterator steps_begin, step::elementary_step_seq::const_iterator steps_end,
      piece::color_t current_player, piece::color_t next_player) const;
  
  /**
   * Same as above, except this function accepts a reference to the sequence of elementary steps, instead of a pair
   * of iterators.
   */
  hash_t update(hash_t old_hash,
      step::elementary_step_seq const& steps,
      piece::color_t current_player, piece::color_t next_player) const;

private:
  friend class zobrist_hasher_pickle;

  typedef boost::multi_array<hash_t, 4>   codes_cont;
  typedef boost::array<hash_t, 2>         players_cont;

  codes_cont    codes;
  players_cont  players;
};

class vertex;

/**
 * A transposition table serves as a cache for already explored positions -- vertices of the search tree. Vertices are indexed
 * by their hash values as given by the supplied Hasher algorithm.
 *
 * If the user code attempts store a vertex with a hash that's already in use, the new vertex will only replace the old one
 * if the old one has been in the table for too long. How long is too long is specified by the \c keep_time parameter. Time is
 * measured in iterations of the main algorithm loop. The timer is updated by calling the #tick function at the start of each
 * iteration.
 *
 * The table allocates its memory in 1MB-sized pages in order to avoid huge allocation at the start of the algorithm.
 *
 * \tparam Hasher Hashing algorithm that is used to make the keys.
 */
class transposition_table : boost::noncopyable {
private:
  typedef unsigned iteration_t;

  //! One record in the table.
  struct record {
    ::vertex*   vertex;
    iteration_t last_accessed;

    record() : vertex(0), last_accessed(0) { }
  };

public:
  typedef zobrist_hasher::hash_t hash_t;
  static std::size_t const SIZE_OF_ELEMENT;  //!< Size, in bytes, of one element of the table.

  /**
   * Create a transposition table.
   * \param table_size Capacity, in number of elements, of this table.
   * \param keep_time How long should a record be kept before it can be replaced by another one?
   */
  transposition_table(std::size_t table_size, std::size_t keep_time);

  /**
   * Insert an element to the table.
   * \param hash Key of the element.
   * \param vertex Value of the element.
   */
  void insert(hash_t hash, vertex* vertex);

  /**
   * Try to retreive an element from the table.
   * \param hash The key.
   * \returns Either the found element or an empty pointer if the element wasn't found.
   */
  vertex* query(hash_t hash);

  /**
   * Inform the table that a query was a hit. That is, the table returned an expected vertex.
   */
  void hit();

  /**
   * Inform the table that a query was a miss.
   */
  void miss();

  //! Update the internal iteration-count timer. Call this each iteration of the search algorithm.
  void tick();

  std::size_t get_memory_usage() const;                     //!< Get the amount of memory, in bytes, of this table.
  std::size_t get_elements() const    { return elements; }  //!< Get the number of elements currently stored in the table.
  std::size_t get_hits() const        { return hits; }      //!< Get the number of successful retreivals from the table.
  std::size_t get_misses() const      { return misses; }    //!< Get the number of unsuccessful retreival attempts.

private:
  static std::size_t const PAGE_SIZE = 1024 * 1024;  //!< One megabyte.
  static std::size_t const PAGE_RECORDS = PAGE_SIZE / sizeof(record);  //!< Number of records that can fit into one page.

  typedef boost::array<record, PAGE_RECORDS> page;  //!< One page of records.
  typedef boost::scoped_ptr<page> page_ptr;         //!< Pointer to page.
  typedef boost::scoped_array<page_ptr> directory;  //!< Directory of pages.

  std::size_t page_number(std::size_t index) const;  //!< Given an index of an element, get the index into the directory.
  std::size_t page_offset(std::size_t index) const;  //!< Given an index of an element, get the index into the page.

  std::size_t const table_size;    //!< How many elements are there in one page?
  std::size_t const keep_time;     //!< How long to keep an element before it can be replaced?
  std::size_t const pages;         //!< How many pages are there?

  directory table;                 //!< The table itself.
  iteration_t current_iteration;   //!< Current "time" measured in iterations of the search algorithm.

  std::size_t allocated_pages;     //!< Number of pages allocated.
  std::size_t elements;            //!< Number of elements stored.
  std::size_t hits;                //!< Number of successful retreivals from the table.
  std::size_t misses;              //!< Number of unsuccessful retreival attempts.
};

#endif
