#include "hash.hpp"
#include "board.hpp"
#include "util.hpp"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>

#include <cstdlib>
#include <limits>

namespace {

typedef boost::multi_array_types::index_range range;

double uniform_deviate(int seed) {
  return seed * (1.0 / (RAND_MAX + 1.0));
}

} // anonymous namespace

zobrist_hasher::zobrist_hasher() 
  : codes(boost::extents[piece::type_count][piece::color_count][board::ROWS][board::COLUMNS])
{
  hash_t const MAX_VALUE = std::numeric_limits<hash_t>::max();

  boost::random::mt19937 prng;
  boost::random::uniform_int_distribution<hash_t> rand_distrib;

  for (std::size_t type = 0; type < piece::type_count; ++type)
    for (std::size_t color = 0; color < piece::color_count; ++color)
      for (std::size_t row = board::MIN_ROW; row <= board::MAX_ROW; ++row)
        for (std::size_t column = board::MIN_COLUMN; column <= board::MAX_COLUMN; ++column)
          codes[type]
               [color]
               [row - board::MIN_ROW]
               [column - board::MIN_COLUMN] = rand_distrib(prng);

  for (std::size_t player = 0; player < piece::color_count; ++player)
    players[player] = static_cast<hash_t>(uniform_deviate(rand()) * MAX_VALUE);
}

zobrist_hasher::hash_t zobrist_hasher::generate_initial(board const& board, piece::color_t on_move) const {
  hash_t hash = 0;

  for (board::pieces_iterator p = board.pieces_begin(); p != board.pieces_end(); ++p) {
    position const& position = p->first;
    piece const& piece = p->second;

    hash ^= codes[piece.get_type()]
                 [piece.get_color()]
                 [position.get_row() - board::MIN_ROW]
                 [position.get_column() - board::MIN_COLUMN];
  }

  hash ^= players[on_move];

  return hash;
}

zobrist_hasher::hash_t zobrist_hasher::update(hash_t old_hash,
    step::elementary_step_seq::const_iterator steps_begin, step::elementary_step_seq::const_iterator steps_end,
    piece::color_t current_player, piece::color_t next_player) const {
  hash_t hash = old_hash;

  for (step::elementary_step_seq::const_iterator step = steps_begin; step != steps_end; ++step) {
    position const& old_position = step->get_from();
    piece const& piece = *step->get_what();  // Assumed to be non-empty.

    // First remove the piece from its old position. If this is a displacement, add the piece's new position to the
    // hash later.
    hash ^= codes[piece.get_type()]
                 [piece.get_color()]
                 [old_position.get_row() - board::MIN_ROW]
                 [old_position.get_column() - board::MIN_COLUMN];

    if (!step->is_capture()) {
      position new_position = make_adjacent(old_position, step->get_where());
      hash ^= codes[piece.get_type()]
                   [piece.get_color()]
                   [new_position.get_row() - board::MIN_ROW]
                   [new_position.get_column() - board::MIN_COLUMN];
    }
  }

  hash ^= players[current_player];
  hash ^= players[next_player];

  return hash;
}

zobrist_hasher::hash_t zobrist_hasher::update(hash_t old_hash,
    step::elementary_step_seq const& steps,
    piece::color_t current_player, piece::color_t next_player) const {
  return update(old_hash, steps.begin(), steps.end(), current_player, next_player);
}

std::size_t const transposition_table::SIZE_OF_ELEMENT = sizeof(record);

transposition_table::transposition_table(std::size_t table_size, std::size_t keep_time)
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

void transposition_table::insert(hash_t hash, entry_t entry) {
  record& r = find_record(hash);
  if (r.last_accessed == NEVER || now - r.last_accessed > keep_time) {
    r.entry = entry;
    r.last_accessed = now;
    ++elements;
  }
}

void transposition_table::update(hash_t hash, entry_t entry) {
  record& r = find_record(hash);
  if (r.last_accessed != NEVER) {
    r.entry = entry;
    r.last_accessed = now;
  }
}

boost::optional<transposition_table::entry_t> transposition_table::query(hash_t hash) {
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

void transposition_table::tick() {
  ++now;
}

std::size_t transposition_table::get_memory_usage() const {
  return allocated_pages * PAGE_SIZE + pages * sizeof(page_ptr);
}

std::size_t transposition_table::page_number(std::size_t index) const {
  return index / PAGE_RECORDS;
}

std::size_t transposition_table::page_offset(std::size_t index) const {
  return index % PAGE_RECORDS;
}

transposition_table::record& transposition_table::find_record(hash_t hash) {
  page_ptr& pg = table[page_number(hash % table_size)];

  if (pg == 0) {
    pg.reset(new page);
    ++allocated_pages;
  }

  return (*pg)[page_offset(hash % table_size)];
}

