#include <algorithm>
#include <limits>
#include <memory>

#include "tree.hpp"

vertex::number_t const vertex::max_num = std::numeric_limits<vertex::number_t>::max();
vertex::number_t const vertex::infty   = std::numeric_limits<vertex::number_t>::max() - 1;

vertex::vertex() :
  proof_number(0),
  disproof_number(0),
  steps_remaining(0),
  type(vertex::type_or),

  storage(0),
  size(0),
  capacity(0)
{ }

vertex::~vertex() {
  resize(0);
}

vertex::children_iterator vertex::add_child() {
  resize(size + 1);
  return children_end() - 1;
}

void vertex::remove_child(children_iterator child) {
  vertex& to_remove = *child;
  children_iterator next_child = child + 1;

  while (next_child < children_end()) 
    *child++ = *next_child++;

  *child = to_remove;

  resize(size - 1);
}

void vertex::reserve(std::size_t new_size) {
  if (new_size > capacity)
    realloc(new_size);
}

void vertex::resize(std::size_t new_size) {
  if (capacity < new_size)
    reserve(new_size);

  if (new_size > size)
    for (std::size_t index = size; index < new_size; ++index)
      new (&get(index)) vertex();

  else if (new_size < size)
    for (std::size_t index = new_size; index < size; ++index)
      get(index).~vertex();

  size = new_size;
}

void vertex::pack() {
  if (capacity > size)
    realloc(size);
}

char* vertex::allocator::allocate(std::size_t n) throw () {
  return new (std::nothrow) char[n * sizeof(vertex)];
}

void vertex::allocator::deallocate(char* memory) throw () {
  delete [] memory;
}

void vertex::storage_wrapper::reset(element_type* new_ptr) throw () {
  allocator::deallocate(storage);
  storage = new_ptr;
}

void vertex::storage_wrapper::swap(storage_wrapper& other) throw () {
  std::swap(storage, other.storage);
}

vertex::vertex(vertex& other) :
  proof_number(other.proof_number),
  disproof_number(other.disproof_number),
  step(other.step),
  steps_remaining(other.steps_remaining),
  type(other.type),

  size(0),
  capacity(0)
{ 
  storage.swap(other.storage);
  std::swap(size, other.size);
  std::swap(capacity, other.capacity);
}

vertex& vertex::operator = (vertex& other) {
  if (this != &other) {
    proof_number = other.proof_number;
    disproof_number = other.disproof_number;
    step = other.step;
    steps_remaining = other.steps_remaining;
    type = other.type;

    std::swap(size, other.size);
    std::swap(capacity, other.capacity);
    storage.swap(other.storage);
  }

  return *this;
}

void vertex::realloc(std::size_t new_alloc) {
  storage_wrapper new_storage;

  if (new_alloc > 0) {
    new_storage.reset(allocator::allocate(new_alloc));

    // *Move* children into new storage.
    for (std::size_t index = 0; index < size; ++index) {
      new (new_storage.get() + index * sizeof(vertex)) vertex(get(index));
      get(index).~vertex();  // Won't deallocate children as the storage of that child is null already.
    }
  }

  storage.swap(new_storage);
  capacity = new_alloc;
}

vertex& vertex::get(std::size_t index) {
  return *(children_begin() + index);
}

