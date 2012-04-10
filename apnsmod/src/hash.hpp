/**
 * \file hash.hpp
 *
 * Hashing algorithms and the transposition table.
 */

#ifndef HASH_HPP
#define HASH_HPP

#include "board.hpp"
#include "tree.hpp"
#include "movement.hpp"

#include <boost/array.hpp>
#include <boost/multi_array.hpp>
#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/utility.hpp>
#include <boost/optional.hpp>
#include <boost/unordered_map.hpp>

#ifdef BOOST_NO_INT64_T
// Defined only if the underlying platform doesn't provide 64-bit integer types. This program can't work without
// 64-bit integer types.
# error "Your platform does not support 64-bit integer types. Sorry."
#endif

namespace apns {

/**
 * Zobrist's algorithm to generate hashes for game positions. This hasher is copyable and assignable, but neither
 * copying nor assigning is cheap.
 */
class zobrist_hasher {
public:
  typedef boost::uint64_t hash_t;

  /**
   * Initialise the hasher. This constructor will generate 770 random numbers and store them in memory, so the construction
   * is heavy.
   */
  zobrist_hasher();

  /**
   * Generate the initial hash for the given board. This is meant to be only called once and then updated through
   * the update function.
   *
   * \param board The board for which the hash will be calculated.
   * \param on_move Whose turn is it?
   * \param steps_remaining How many steps does the player have remaining?
   * \return The hash value.
   */
  hash_t generate_initial(board const& board, piece::color_t on_move) const;

  /**
   * Update the hash value after the situation on board has changed.
   *
   * \param old_hash Previous value of the hash.
   * \param steps_begin Beginning of a sequence of elementary steps that describe the movement of the pieces on the board.
   * \param steps_end End of the sequence of elementary steps.
   * \param current_steps_remaining How many steps were remaining for the current player before having made this step?
   * \param next_steps_remaining How many steps remain for the next player after having made this step?
   * \param current_player Who made the steps described?
   * \param next_player Whose turn is it now?
   */
  template <typename Iter>
    hash_t update(hash_t old_hash, Iter steps_begin, Iter steps_end, 
                  piece::color_t current_player, piece::color_t next_player) const;

  //! Given a hash, return the hash value corresponding to the same board state, but with the opposite player to move.
  hash_t opponent_hash(hash_t h) const {
    h ^= players[0];
    h ^= players[1];
    return h;
  }

private:
  typedef boost::multi_array<hash_t, 4>   codes_cont;
  typedef boost::array<hash_t, 2>         players_cont;
  typedef boost::array<hash_t, 4>         steps_cont;

  codes_cont    codes;
  players_cont  players;
};

template <typename Iter>
zobrist_hasher::hash_t zobrist_hasher::update(hash_t old_hash, Iter steps_begin, Iter steps_end, 
                                              piece::color_t current_player, piece::color_t next_player) const {
  hash_t hash = old_hash;

  for (Iter step = steps_begin; step != steps_end; ++step) {
    position const& old_position = step->get_from();
    piece const& piece = *step->get_what();  // Assumed to be non-empty.

    // First remove the piece from its old position. If this is a displacement, add the piece's new position to the
    // hash later.
    hash ^= codes[index_from_type(piece.get_type())]
                 [index_from_color(piece.get_color())]
                 [old_position.get_row() - board::MIN_ROW]
                 [old_position.get_column() - board::MIN_COLUMN];

    if (!step->is_capture()) {
      position new_position = make_adjacent(old_position, step->get_where());
      hash ^= codes[index_from_type(piece.get_type())]
                   [index_from_color(piece.get_color())]
                   [new_position.get_row() - board::MIN_ROW]
                   [new_position.get_column() - board::MIN_COLUMN];
    }
  }

  hash ^= players[current_player];
  hash ^= players[next_player];

  return hash;
}

namespace detail {

//! Storage of records that can be accessed by their Hash value. Collisions are ignored on query. If a collision occurs
//! on insertion, and the old entry was not recently accessed, it is replaced; otherwise, the new value is not stored.
template <typename Entry, typename Hash>
class table : boost::noncopyable {
public:
  //! Type of values stored in the table.
  typedef Entry entry_t;
  typedef Hash  hash_t;
  static std::size_t const SIZE_OF_ELEMENT;  //!< Size, in bytes, of one element of the table.
  
private:
  //! One record in the table.
  struct record {
    entry_t     entry;
    int         depth;
    hash_t      hash;

    record() : depth(-1), hash(0) { }

    bool is_set() {
      return depth != -1;
    }
  };

public:
  /**
   * Create a transposition table.
   * \param table_size Capacity, in number of elements, of this table.
   * \param keep_time How long should a record be kept before it can be replaced by another one?
   */
  table(std::size_t table_size)
    : table_size(table_size)
    , pages((table_size / PAGE_RECORDS) + (table_size % PAGE_RECORDS != 0 ? 1 : 0))  // pages := ceil(table_size / PAGE_RECORDS)
    , dir(new page_ptr[pages])
    , allocated_pages(0)
    , elements(0)
    , hits(0)
    , misses(0)
  {
    assert(sizeof(page) <= PAGE_SIZE);
    assert(pages * PAGE_RECORDS >= table_size);
  }

  /**
   * Insert an element to the table.
   * \param hash Key of the element.
   * \param vertex Value of the element.
   */
  void insert(hash_t hash, int depth, entry_t entry) {
    record& r = find_record(hash);
    if (!r.is_set() || r.hash == hash || depth < r.depth) {
      if (!r.is_set())
        ++elements;
      r.entry = entry;
      r.depth = depth;
      r.hash = hash;
    }
  }

  /**
   * Try to retreive an element from the table.
   * \param hash The key.
   * \returns Either the found element or an empty pointer if the element wasn't found.
   */
  boost::optional<entry_t> query(hash_t hash) {
    record& r = find_record(hash);
    if (r.is_set() && r.hash == hash) {
      ++hits;
      return r.entry;
    } else {
      ++misses;
      return boost::none;
    }
  }

  std::size_t get_memory_usage() const {                      //!< Get the amount of memory, in bytes, of this table.
    return allocated_pages * PAGE_SIZE + pages * sizeof(page_ptr);
  }

  std::size_t get_table_size() const  { return table_size; }  //!< Get the maximal number of elements storeable in this table.
  std::size_t get_elements() const    { return elements; }    //!< Get the number of elements currently stored in the table.
  std::size_t get_hits() const        { return hits; }        //!< Get the number of successful retreivals from the table.
  std::size_t get_misses() const      { return misses; }      //!< Get the number of unsuccessful retreival attempts.

private:
  static std::size_t const PAGE_SIZE = 1024 * 1024;  //!< One megabyte.
  static std::size_t const PAGE_RECORDS = PAGE_SIZE / sizeof(record);  //!< Number of records that can fit into one page.

  typedef boost::array<record, PAGE_RECORDS> page;  //!< One page of records.
  typedef boost::scoped_ptr<page> page_ptr;         //!< Pointer to page.
  typedef boost::scoped_array<page_ptr> directory;  //!< Directory of pages.

  std::size_t page_number(std::size_t index) const {  //!< Given an index of an element, get the index into the directory.
    return index / PAGE_RECORDS;
  }
  std::size_t page_offset(std::size_t index) const {  //!< Given an index of an element, get the index into the page.
    return index % PAGE_RECORDS;
  }
  
  //! Get the record for given hash.
  record& find_record(hash_t hash) {
    page_ptr& pg = dir[page_number(hash % table_size)];

    if (pg == 0) {
      pg.reset(new page);
      ++allocated_pages;
    }

    return (*pg)[page_offset(hash % table_size)];
  }

  std::size_t const table_size;    //!< How many elements are there in one page?
  std::size_t const pages;         //!< How many pages are there?

  directory dir;                   //!< The table itself.

  std::size_t allocated_pages;     //!< Number of pages allocated.
  std::size_t elements;            //!< Number of elements stored.
  std::size_t hits;                //!< Number of successful retreivals from the table.
  std::size_t misses;              //!< Number of unsuccessful retreival attempts.
};

template <typename Entry, typename Hash>
std::size_t const table<Entry, Hash>::SIZE_OF_ELEMENT = sizeof(record);

} // namespace detail

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
 */
typedef detail::table<std::pair<vertex::number_t, vertex::number_t>, zobrist_hasher::hash_t> transposition_table;

//! History of a path is a collection of the positions that occured at the start of each player's turn.
typedef boost::unordered_map<zobrist_hasher::hash_t, std::size_t> history_t;

//! An entry in the proof table.
struct proof_entry_t {
  vertex::number_t  proof_number;
  vertex::number_t  disproof_number;
  history_t         history;
};

typedef detail::table<proof_entry_t, zobrist_hasher::hash_t> proof_table;

} // namespace apns

#endif

