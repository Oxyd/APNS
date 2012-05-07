/**
 * \file hash.hpp
 *
 * Hashing algorithms and the transposition table.
 */

#ifndef HASH_HPP
#define HASH_HPP

#include "board.hpp"
#include "tree.hpp"

#include <boost/array.hpp>
#include <boost/multi_array.hpp>
#include <boost/cstdint.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/utility.hpp>
#include <boost/optional.hpp>
#include <boost/unordered_map.hpp>

#include <limits>

#ifdef BOOST_NO_INT64_T
// Defined only if the underlying platform doesn't provide 64-bit integer 
// types. This program can't work without
// 64-bit integer types.
# error "Your platform does not support 64-bit integer types. Sorry."
#endif

namespace apns {

/**
 * Zobrist's algorithm to generate hashes for game positions. This hasher is 
 * copyable and assignable, but neither copying nor assigning is cheap.
 */
class zobrist_hasher {
public:
  typedef boost::uint64_t hash_t;

  /**
   * Initialise the hasher. This constructor will generate 770 random numbers 
   * and store them in memory, so the construction is heavy.
   */
  zobrist_hasher();

  /**
   * Generate the initial hash for the given board. This is meant to be only 
   * called once and then updated through the update function.
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
   * \param steps_begin Beginning of a sequence of elementary steps that 
   *   describe the movement of the pieces on the board.
   * \param steps_end End of the sequence of elementary steps.
   * \param current_steps_remaining How many steps were remaining for the 
   *   current player before having made this step?
   * \param next_steps_remaining How many steps remain for the next player 
   *   after having made this step?
   * \param current_player Who made the steps described?
   * \param next_player Whose turn is it now?
   */
  template <typename Iter>
    hash_t update(
      hash_t old_hash, Iter steps_begin, Iter steps_end, 
      piece::color_t current_player, piece::color_t next_player
    ) const;

  //! Given a hash, return the hash value corresponding to the same board 
  //! state, but with the opposite player to move.
  hash_t opponent_hash(hash_t h) const {
    h ^= players_[0];
    h ^= players_[1];
    return h;
  }

private:
  typedef boost::multi_array<hash_t, 4>   codes_cont;
  typedef boost::array<hash_t, 2>         players_cont;
  typedef boost::array<hash_t, 4>         steps_cont;

  codes_cont    codes_;
  players_cont  players_;
};

template <typename Iter>
zobrist_hasher::hash_t zobrist_hasher::update(
  hash_t old_hash, Iter steps_begin, Iter steps_end, 
  piece::color_t current_player, piece::color_t next_player
) const {
  hash_t hash = old_hash;

  for (Iter step = steps_begin; step != steps_end; ++step) {
    position const& old_position = step->from();
    piece const& piece = *step->what();  // Assumed to be non-empty.

    // First remove the piece from its old position. If this is a displacement,
    // add the piece's new position to the hash later.
    hash ^= codes_[index_from_type(piece.type())]
                  [index_from_color(piece.color())]
                  [old_position.row() - board::MIN_ROW]
                  [old_position.column() - board::MIN_COLUMN];

    if (!step->capture()) {
      position new_position = make_adjacent(old_position, step->where());
      hash ^= codes_[index_from_type(piece.type())]
                    [index_from_color(piece.color())]
                    [new_position.row() - board::MIN_ROW]
                    [new_position.column() - board::MIN_COLUMN];
    }
  }

  hash ^= players_[current_player];
  hash ^= players_[next_player];

  return hash;
}

namespace detail {

//! Storage of records that can be accessed by their Hash value. Hash 
//! collisions are ignored, however the Hash % N collisions are handled in an
//! appropriate way.
//!
//! Should a Hash % N collision happen on insert, the depth value is used to
//! determine which record to keep.
template <typename Entry, typename Hash>
class table : boost::noncopyable {
public:
  //! Type of values stored in the table.
  typedef Entry entry_t;
  typedef Hash  hash_t;

  //! Size, in bytes, of one element of the table.
  static std::size_t const SIZE_OF_ELEMENT;  
  
private:
  //! One record in the table.
  struct record {
    entry_t   entry;
    unsigned  depth;
    hash_t    hash;

    record() : depth(UNSET), hash(0) { }

    bool is_set() {
      return depth != UNSET;
    }

  private:
    static unsigned const UNSET;
  };

public:
  /**
   * Create a transposition table.
   * \param table_size Capacity, in number of elements, of this table.
   * \param keep_time How long should a record be kept before it can be 
   *   replaced by another one?
   */
  table(std::size_t table_size)
    : table_size_(table_size)

    // pages := ceil(table_size / PAGE_RECORDS)
    , pages_((table_size / PAGE_RECORDS_) + 
             (table_size % PAGE_RECORDS_ != 0 ? 1 : 0))

    , dir_(new page_ptr[pages_])
    , allocated_pages_(0)
    , elements_(0)
    , hits_(0)
    , misses_(0)
  {
    assert(sizeof(page) <= PAGE_SIZE_);
    assert(pages_ * PAGE_RECORDS_ >= table_size_);
  }

  /**
   * Insert an element to the table.
   * \param hash Key of the element.
   * \param vertex Value of the element.
   */
  void insert(hash_t hash, unsigned depth, entry_t entry) {
    record* r = find_record(hash, true);
    assert(r);

    if (!r->is_set() || r->hash == hash || depth < r->depth) {
      if (!r->is_set())
        ++elements_;
      r->entry = entry;
      r->depth = depth;
      r->hash = hash;
    }
  }

  /**
   * Try to retreive an element from the table.
   * \param hash The key.
   * \returns Either the found element or nothing if the element wasn't found.
   */
  boost::optional<entry_t> query(hash_t hash) {
    record* r = find_record(hash, false);
    if (r && r->is_set() && r->hash == hash) {
      ++hits_;
      return r->entry;
    } else {
      ++misses_;
      return boost::none;
    }
  }

  //! Inform the table that the client code rejected the last successful query 
  //! and that is should thus be counted as a miss
  //! rather than a hit.
  void reject() {
    assert(hits_ > 0);

    --hits_;
    ++misses_;
  }

  //! Get the amount of memory, in bytes, of this table.
  std::size_t memory_usage() const {                       
    return allocated_pages_ * PAGE_SIZE_ + pages_ * sizeof(page_ptr);
  }

  //! Get the maximal number of elements storeable in this table.
  std::size_t table_size() const  { return table_size_; }  

  //! Get the number of elements currently stored in the table.
  std::size_t elements() const    { return elements_; }    

  //! Get the number of successful retreivals from the table.
  std::size_t hits() const        { return hits_; }        

  //! Get the number of unsuccessful retreival attempts.
  std::size_t misses() const      { return misses_; }      

private:
  static std::size_t const PAGE_SIZE_ = 1024 * 1024;  //!< One megabyte.

  //! Number of records that can fit into one page.
  static std::size_t const PAGE_RECORDS_ = PAGE_SIZE_ / sizeof(record);  

  typedef boost::array<record, PAGE_RECORDS_> page;  //!< One page of records.
  typedef boost::scoped_ptr<page> page_ptr;         //!< Pointer to page.
  typedef boost::scoped_array<page_ptr> directory;  //!< Directory of pages_.

  //! Given an index of an element, get the index into the directory.
  std::size_t page_number(std::size_t index) const {  
    return index / PAGE_RECORDS_;
  }

  //! Given an index of an element, get the index into the page.
  std::size_t page_offset(std::size_t index) const {  
    return index % PAGE_RECORDS_;
  }
  
  //! Get the record for given hash.
  record* find_record(hash_t hash, bool allocate) {
    page_ptr& pg = dir_[page_number(hash % table_size_)];

    if (pg == 0 && allocate) {
      pg.reset(new page);
      ++allocated_pages_;
    }

    if (pg != 0)
      return &((*pg)[page_offset(hash % table_size_)]);
    else
      return 0;
  }

  std::size_t const table_size_;  //!< How many elements are there in one page?
  std::size_t const pages_;       //!< How many pages are there?

  directory dir_;                 //!< The table itself.

  std::size_t allocated_pages_;   //!< Number of pages allocated.
  std::size_t elements_;          //!< Number of elements stored.
  std::size_t hits_;       //!< Number of successful retreivals from the table.
  std::size_t misses_;     //!< Number of unsuccessful retreival attempts.
};

template <typename Entry, typename Hash>
std::size_t const table<Entry, Hash>::SIZE_OF_ELEMENT = sizeof(record);

template <typename Entry, typename Hash>
unsigned const table<Entry, Hash>::record::UNSET =
  std::numeric_limits<unsigned>::max();

} // namespace detail

struct transposition_entry {
  vertex::number_t  proof_number;
  vertex::number_t  disproof_number;

  transposition_entry() : proof_number(0), disproof_number(0) { }
  transposition_entry(vertex::number_t pn, vertex::number_t dn) :
    proof_number(pn), disproof_number(dn)
  { }
};

//! A transposition table stores visited positions for later recall.
typedef detail::table<transposition_entry, zobrist_hasher::hash_t> 
  transposition_table;

//! History of a path is a collection of the positions that occured at the 
//! start of each player's turn.
typedef std::vector<zobrist_hasher::hash_t> history_t;

//! An entry in the proof table.
struct proof_entry_t {
  vertex::number_t  proof_number;
  vertex::number_t  disproof_number;
  history_t         history;

  proof_entry_t() : proof_number(0), disproof_number(0) { }
  proof_entry_t(vertex::number_t pn, vertex::number_t dn, history_t const& h) :
    proof_number(pn), disproof_number(dn), history(h)
  { }
};

typedef detail::table<proof_entry_t, zobrist_hasher::hash_t> proof_table;

} // namespace apns

#endif

