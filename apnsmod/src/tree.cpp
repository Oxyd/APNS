#include "tree.hpp"

#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/bind.hpp>

#include <algorithm>
#include <limits>
#include <memory>
#include <string>
#include <fstream>
#include <iostream>
#include <stack>
#include <utility>

namespace {

//! Postorder traveral policy for traverse_postorder. Do note that this never traverses the root vertex.
class postorder {
  typedef std::stack<std::pair<vertex::children_iterator, vertex::children_iterator> > stack_t;

public:
  vertex::children_iterator operator () (vertex& v) {
    if (stack.empty())
      return recurse(&v)++;

    if (stack.top().first != stack.top().second)
      return recurse(stack.top().first)++;
    else {
      while (!stack.empty() && stack.top().first == stack.top().second)
        stack.pop();

      if (!stack.empty())
        return stack.top().first++;
      else
        return vertex::children_iterator();
    }
  }

private:
  stack_t stack;

  vertex::children_iterator& recurse(vertex::children_iterator from) {
    vertex::children_iterator current = from;
    while (current->children_count() > 0) {
      stack.push(std::make_pair(current->children_begin(), current->children_end()));
      current = current->children_begin();
    }

    return stack.top().first;
  }
};

} // anonymous namespace

vertex::number_t const vertex::max_num = std::numeric_limits<vertex::number_t>::max();
vertex::number_t const vertex::infty   = std::numeric_limits<vertex::number_t>::max() - 1;

std::size_t vertex::count = 0;

vertex::vertex() :
  proof_number(0),
  disproof_number(0),
  steps_remaining(0),
  type(vertex::type_or),

  storage(0),
  size(0),
  capacity(0)
{
  ++count;
}

vertex::~vertex() {
  --count;

  if (children_count() > 0)
    traverse_postorder(*this, postorder(), boost::bind(&vertex::destroy, _1));
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

void vertex::swap(vertex& other) {
  storage.swap(other.storage);
  std::swap(size, other.size);
  std::swap(capacity, other.capacity);

  std::swap(proof_number, other.proof_number);
  std::swap(disproof_number, other.disproof_number);
  std::swap(step, other.step);
  std::swap(steps_remaining, other.steps_remaining);
  std::swap(type, other.type);
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
  ++count;
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

void vertex::destroy() {
  capacity = size = 0;
  storage.reset();
  this->~vertex();
}

namespace {

class backtrack {
  // A stack of (current vertex on a level, end iterator for that level).
  typedef std::stack<std::pair<vertex::children_iterator, vertex::children_iterator> > stack_t;

public:
  vertex::children_iterator operator () (vertex& current) {
    if (current.children_count() > 0) {
      // This vertex has any children? Good, go to the first one. Also push this level onto the stack.
      stack.push(std::make_pair(current.children_begin(), current.children_end()));
      return current.children_begin();

    } else 
      while (!stack.empty()) {
        // A leaf? Okay, does it have an unvisited sibling?
        vertex::children_iterator& cur = stack.top().first;
        vertex::children_iterator& end = stack.top().second;

        ++cur;
        if (cur != end)
          return cur;   // It does, visit that.
        else
          stack.pop();  // Nope, try one level above.
      }

    // No dice either way? Looks like we're done.
    return vertex::children_iterator();
  }

private:
  stack_t stack;
};

//! A stop condition that just keeps calling op_ctrl.update() to see whether the algorithm should stop.
struct op_ctrl_stop_cond {
  explicit op_ctrl_stop_cond(operation_controller& op_ctrl) :
    op_ctrl(op_ctrl)
  { }

  bool operator () (vertex&) {
    op_ctrl.update();
    return op_ctrl.stop();
  }

private:
  operation_controller& op_ctrl;
};

void bad_format() {
  throw std::runtime_error("Bad file format");
}

//! Dump-a-vertex visitor.
struct printer {
  explicit printer(std::ostream& out) :
    out(out)
  { }

  void operator () (vertex& v) {
    out << (v.step ? v.step->to_string() : "root")
        << " : " << (v.type == vertex::type_or ? "or" : "and")
        << ' '   << v.steps_remaining
        << ' '   << (v.proof_number < vertex::infty ? boost::lexical_cast<std::string>(v.proof_number) : "infty")
        << ' '   << (v.disproof_number < vertex::infty ? boost::lexical_cast<std::string>(v.disproof_number) : "infty")
        << ' '   << v.children_count()
        << '\n'
        ;
  }

private:
  std::ostream& out;
};

//! Load-a-vertex visitor.
struct reader {
  explicit reader(std::istream& in) :
    in(in)
  { }

  void operator () (vertex& v) {
    try {
      std::string token;

      if (!std::getline(in, token, ':')) bad_format();
      boost::algorithm::trim(token);
      boost::optional<step> maybe_step = step::from_string(token);
      if (maybe_step)
        v.step = maybe_step;
      else if (token != "root") bad_format();

      if (!(in >> token)) bad_format();
      if (token == "or") v.type = vertex::type_or;
      else if (token == "and") v.type = vertex::type_and;
      else bad_format();

      vertex::number_t num_token;
      if (!(in >> num_token)) bad_format();
      if (num_token && num_token <= 4) v.steps_remaining = num_token;
      else bad_format();

      if (!(in >> token)) bad_format();
      if (token != "infty") v.proof_number = boost::lexical_cast<vertex::number_t>(token);
      else v.proof_number = vertex::infty;

      if (!(in >> token)) bad_format();
      if (token != "infty") v.disproof_number = boost::lexical_cast<vertex::number_t>(token);
      else v.disproof_number = vertex::infty;

      if (!(in >> num_token)) bad_format();
      v.resize(num_token);
    } catch (boost::bad_lexical_cast&) {
      bad_format();
    }
  }

private:
  std::istream& in;
};

//! A visitor counting the vertices of the tree.
struct vertex_counter {
  vertex_counter() : count(0) { }

  void operator () (vertex&) {
    ++count;
  }

  std::size_t count;
};

//! Delete a file given by its filename. This is inherently platform-specific.
void delete_file(std::string const& filename);

} // anonymous namespace

#ifdef POSIX
#include <unistd.h>

namespace {

void delete_file(std::string const& filename) {
  ::unlink(filename.c_str());
}

} // anonymous namespace

#elif defined(_WIN32)
#include <windows.h>

namespace {

void delete_file(std::string const& filename) {
  ::DeleteFile(filename.c_str());
}

} // anonymous namespace

#else
# error "Unsupported platform"
#endif

void save_game(boost::shared_ptr<game> const& game, std::string const& filename, operation_controller& op_ctrl) {
  std::ios_base::sync_with_stdio(false);

  std::ofstream out(filename.c_str());
  try {
    out.exceptions(std::ios_base::badbit | std::ios_base::failbit);
    
    out << string_from_board(game->initial_state);
    out << '\n';
    if (game->attacker == piece::gold)
      out << "gold";
    else
      out << "silver";
    out << '\n';

    traverse(game->root, backtrack(), printer(out), op_ctrl_stop_cond(op_ctrl));

    if (op_ctrl.stop())
      delete_file(filename);  // Cancelled -- the file has been only partially written.

  } catch (std::ios_base::failure& f) {
    delete_file(filename);  // The file has only been partially written.
    throw std::runtime_error(f.what());
  }
}

std::pair<boost::shared_ptr<game>, std::size_t> load_game(std::string const& filename, operation_controller& op_ctrl) {
  std::ios_base::sync_with_stdio(false);

  std::ifstream in(filename.c_str());
  in.exceptions(std::ios_base::badbit);

  std::string line;
  if (!std::getline(in, line)) bad_format();
  board initial_state;
  board_from_string(line, initial_state);

  if (!std::getline(in, line)) bad_format();
  boost::algorithm::trim(line);

  piece::color_t attacker;
  if (line == "gold")         attacker = piece::gold;
  else if (line == "silver")  attacker = piece::silver;
  else bad_format();

  boost::shared_ptr< ::game> game(new ::game(initial_state, attacker));
  vertex_counter counter;
  traverse(game->root, backtrack(),
           make_composite_visitor(reader(in), boost::ref(counter)),
           op_ctrl_stop_cond(op_ctrl));

  if (!op_ctrl.stop())
    return std::make_pair(game, counter.count);
  else
    return std::make_pair(boost::shared_ptr< ::game>(), 0);
}

