#include "hash.hpp"
#include "board.hpp"
#include "util.hpp"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>

#include <cstdlib>
#include <limits>

namespace {

typedef boost::multi_array_types::index_range range;

} // anonymous namespace

apns::zobrist_hasher::zobrist_hasher() 
  : codes(boost::extents[piece::type_count][piece::color_count][board::ROWS][board::COLUMNS])
{
  boost::random::mt19937 prng;
  boost::random::uniform_int_distribution<hash_t> rand_distrib;

  for (types_array_t::const_iterator type = TYPES.begin(); type != TYPES.end(); ++type)
    for (colors_array_t::const_iterator color = COLORS.begin(); color != COLORS.end(); ++color)
      for (std::size_t row = board::MIN_ROW; row <= board::MAX_ROW; ++row)
        for (std::size_t column = board::MIN_COLUMN; column <= board::MAX_COLUMN; ++column)
          codes[type - TYPES.begin()]
               [color - COLORS.begin()]
               [row - board::MIN_ROW]
               [column - board::MIN_COLUMN] = rand_distrib(prng);

  for (std::size_t player = 0; player < piece::color_count; ++player)
    players[player] = static_cast<hash_t>(rand_distrib(prng));
}

apns::zobrist_hasher::hash_t apns::zobrist_hasher::generate_initial(board const& board, piece::color_t on_move) const {
  hash_t hash = 0;

  for (board::pieces_iterator p = board.pieces_begin(); p != board.pieces_end(); ++p) {
    position const& position = p->first;
    piece const& piece = p->second;

    hash ^= codes[index_from_type(piece.get_type())]
                 [index_from_color(piece.get_color())]
                 [position.get_row() - board::MIN_ROW]
                 [position.get_column() - board::MIN_COLUMN];
  }

  hash ^= players[on_move];

  return hash;
}

std::size_t const apns::transposition_table::SIZE_OF_ELEMENT = sizeof(record);

apns::transposition_table::transposition_table(std::size_t table_size, std::size_t keep_time)
  : table_size(table_size)
  , keep_time(keep_time)
  , pages((table_size / PAGE_RECORDS) + (table_size % PAGE_RECORDS != 0 ? 1 : 0))  // pages := ceil(table_size / PAGE_RECORDS)
  , table(new page_ptr[pages])
  , now(1)
  , allocated_pages(0)
  , elements(0)
  , hits(0)
  , misses(0)
{
  assert(sizeof(page) <= PAGE_SIZE);
  assert(pages * PAGE_RECORDS >= table_size);
}

void apns::transposition_table::insert(hash_t hash, entry_t entry) {
  record& r = find_record(hash);
  if (r.last_accessed == NEVER || now - r.last_accessed > keep_time) {
    r.entry = entry;
    r.last_accessed = now;
    ++elements;
  }
}

void apns::transposition_table::update(hash_t hash, entry_t entry) {
  record& r = find_record(hash);
  if (r.last_accessed != NEVER) {
    r.entry = entry;
    r.last_accessed = now;
  }
}

boost::optional<apns::transposition_table::entry_t> apns::transposition_table::query(hash_t hash) {
  record& r = find_record(hash);
  if (r.last_accessed != NEVER) {
    r.last_accessed = now;
    ++hits;

    return r.entry;
  } else {
    ++misses;
    return boost::none;
  }
}

void apns::transposition_table::tick() {
  ++now;
}

std::size_t apns::transposition_table::get_memory_usage() const {
  return allocated_pages * PAGE_SIZE + pages * sizeof(page_ptr);
}

std::size_t apns::transposition_table::page_number(std::size_t index) const {
  return index / PAGE_RECORDS;
}

std::size_t apns::transposition_table::page_offset(std::size_t index) const {
  return index % PAGE_RECORDS;
}

apns::transposition_table::record& apns::transposition_table::find_record(hash_t hash) {
  page_ptr& pg = table[page_number(hash % table_size)];

  if (pg == 0) {
    pg.reset(new page);
    ++allocated_pages;
  }

  return (*pg)[page_offset(hash % table_size)];
}

