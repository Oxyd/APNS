#include "util.hpp"
#include "search.hpp"

#include <iostream>
#include <fstream>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <cstdlib>
#include <map>

namespace {

typedef std::map<vertex_ptr, unsigned> pickle_map;

/**
 * Dump the vertices of the three rooted at 'root' into output. This function will assign each vertex of the tree a unique
 * positive integer and store its value in pickle_numbers. The root of the tree is guaranteed to be assigned number of 1. This function
 * also sets the visited flag of each vertex to true, and expects that all vertices are initially unvisited.
 */
void dump_vertices(std::ostream& output, vertex_ptr root, pickle_map& pickle_numbers, operation_controller& op_ctrl) {
  std::stack<vertex_ptr> stack;
  stack.push(root);

  unsigned number = 1;

  while (!stack.empty() && !op_ctrl.stop()) {
    vertex_ptr vertex = stack.top();
    stack.pop();

    if (!vertex->visited) {
      pickle_numbers[vertex] = number;
      vertex->visited = true;

      std::string const step = vertex->leading_step ? vertex->leading_step->to_string() : "root";
      std::string const type = vertex->type == vertex::type_and ? "and" : "or";

      output << number << ": " << step << ": " << type << " " << vertex->steps_remaining << " ";
      if (vertex->proof_number < vertex::infty) output << vertex->proof_number;
      else                                      output << "infty";
      output << ' ';
      if (vertex->disproof_number < vertex::infty) output << vertex->disproof_number;
      else                                         output << "infty";
      output << '\n';

      for (vertex::children_iterator child = vertex->children_begin(); child != vertex->children_end(); ++child) {
        stack.push(*child);
      }

      ++number;
      op_ctrl.update(op_ctrl.get_work_done() + 1, op_ctrl.get_work_total());
    }
  }
}

/**
 * Dump the edges of the tree rooted at 'root' into output. This function will set the visited flag of each vertex
 * back to false.
 */
void dump_edges(std::ostream& output, vertex_ptr root, pickle_map const& pickle_numbers, operation_controller& op_ctrl) {
  std::stack<vertex_ptr> stack;
  stack.push(root);

  while (!stack.empty() && !op_ctrl.stop()) {
    vertex_ptr vertex = stack.top();
    stack.pop();

    if (vertex->visited) {
      if (vertex->children_begin() != vertex->children_end()) {  // Only actually dump the vertex if it has any children.
        output << pickle_numbers.find(vertex)->second << " : ";

        for (vertex::children_iterator child = vertex->children_begin(); child != vertex->children_end(); ++child) {
          output << pickle_numbers.find(*child)->second << ' ';
          stack.push(*child);
        }
        output << '\n';
      }

      vertex->visited = false;
    }
  }
}

std::string const VERTICES_EDGES_SEPARATOR = "---";

//! Throw a "Loading Tree Failed" error.
void throw_loading_failed() {
  throw std::runtime_error("Loading the search tree failed: Specified file is incorrect or corrupt");
}

/**
 * Load tree vertices from the given list. Returns a list l where l[index] is the vertex that was originally saved with
 * pickle_number == index - 1. The returned vertices have the visited flag set to false.
 *
 * The separator string is consumed from the input.
 */
void load_vertices(std::istream& input, std::vector<vertex_ptr>& vertices, operation_controller& op_ctrl) {
  std::string line;
  while (std::getline(input, line) && !op_ctrl.stop()) {
    if (line != VERTICES_EDGES_SEPARATOR) {
      std::istringstream parser(line);

      unsigned pickle_no;
      parser >> pickle_no;
      if (!parser) throw_loading_failed();

      if (pickle_no - 1 != vertices.size()) throw_loading_failed();

      char delimeter;
      parser >> delimeter;
      if (!parser || delimeter != ':') throw_loading_failed();

      std::auto_ptr<vertex> vertex(new ::vertex);

      std::string step_str;
      std::getline(parser, step_str, ':');
      if (!parser) throw_loading_failed();

      if (step_str.length() == 0) throw_loading_failed();
      if (step_str[0] == ' ') step_str.erase(step_str.begin());

      if (step_str != "root") {
        vertex->leading_step = step::from_string(step_str);
        if (!vertex->leading_step && pickle_no != 1) throw_loading_failed();
      }

      std::string type_str;
      parser >> type_str;
      if (!parser) throw_loading_failed();

      if (type_str == "and")      vertex->type = vertex::type_and;
      else if (type_str == "or")  vertex->type = vertex::type_or;
      else throw_loading_failed();

      parser >> vertex->steps_remaining;
      if (!parser) throw_loading_failed();

      std::string num_str;
      parser >> num_str;
      if (!parser) throw_loading_failed();

      if (num_str == "infty") {
        vertex->proof_number = vertex::infty;
      } else {
        std::istringstream subparser(num_str);
        subparser >> vertex->proof_number;
        if (!subparser) throw_loading_failed();
      }

      parser >> num_str;
      if (!parser) throw_loading_failed();
      if (num_str == "infty") {
        vertex->disproof_number = vertex::infty;
      } else {
        std::istringstream subparser(num_str);
        subparser >> vertex->disproof_number;
        if (!subparser) throw_loading_failed();
      }

      vertices.push_back(vertex.release());
      op_ctrl.update(op_ctrl.get_work_done() + 1, op_ctrl.get_work_total());
    } else {
      break;
    }
  }
}

//! Load edges from input and store them in vertices. vertices is expected to be a list as returned by load_vertices.
void load_edges(std::istream& input, std::vector<vertex_ptr> const& vertices, operation_controller& op_ctrl) {
  std::string line;
  while (std::getline(input, line) && !op_ctrl.stop()) {
    std::istringstream parser(line);

    unsigned vertex_no;
    parser >> vertex_no;
    if (!parser) throw_loading_failed();

    vertex_ptr vertex = vertices[vertex_no - 1];

    char delimeter;
    parser >> delimeter;
    if (!parser || delimeter != ':') throw_loading_failed();

    unsigned edge;
    std::vector<vertex_ptr> children;
    while (parser >> edge) {
      children.push_back(vertices[edge - 1]);
      vertices[edge - 1]->add_parent(vertex);
    }

    vertex->alloc_children(children.size());
    std::copy(children.begin(), children.end(), vertex->children_begin());
  }
}

std::size_t memory_usage = 0;                     //!< Total memory usage of this module.

}

prng_t prng;

void* operator new (std::size_t size) throw (std::bad_alloc) {
  std::size_t const total_size = size + sizeof(std::size_t);
  char* storage = static_cast<char*>(std::malloc(total_size));
  if (storage) {
    *reinterpret_cast<std::size_t*>(storage) = total_size;
    memory_usage += total_size;
    return storage + sizeof(std::size_t);
  } else {
    throw std::bad_alloc();
  }
}

void* operator new [] (std::size_t size) throw (std::bad_alloc) {
  return ::operator new (size);
}

//! Tracking op delete.
void operator delete (void* storage) throw () {
  if (storage) {
    char* const real_storage = static_cast<char*>(storage) - sizeof(std::size_t);
    memory_usage -= *reinterpret_cast<std::size_t*>(real_storage);
    std::free(real_storage);
  }
}

void operator delete [] (void* storage) throw () {
  ::operator delete (storage);
}

std::size_t get_memory_usage() {
  return memory_usage;
}

operation_controller::operation_controller(unsigned ms_update_time)
  : ms_update_time(ms_update_time)
  , stop_requested(false)
  , work_done(0)
  , work_total(0)
{ }

operation_controller::~operation_controller() { }

void operation_controller::update() {
  double const MS = 1000.0;

  if (update_timer.elapsed() >= ms_update_time / MS) {
    do_update();
    update_timer.restart();
  }
}

void operation_controller::update(unsigned done, unsigned total) {
  work_done = done;
  work_total = total;
  update();
}

void operation_controller::finished() {
  do_update();
}

bool operation_controller::stop() const {
  return stop_requested;
}

void operation_controller::request_stop() {
  stop_requested = true;
}

unsigned operation_controller::get_work_done() const {
  return work_done;
}

unsigned operation_controller::get_work_total() const {
  return work_total;
}

void dump_tree(std::string const& filename, vertex_ptr root, bool append, operation_controller& op_ctrl, unsigned tree_size) {
  std::ofstream output(filename.c_str(), append ? std::ios_base::out | std::ios_base::app : std::ios_base::out);
  output.exceptions(std::ios_base::badbit | std::ios_base::failbit);

  op_ctrl.update(0, tree_size);

  pickle_map pickle_numbers;
  dump_vertices(output, root, pickle_numbers, op_ctrl);
  output << VERTICES_EDGES_SEPARATOR << '\n';
  dump_edges(output, root, pickle_numbers, op_ctrl);

  op_ctrl.finished();
}

vertex_ptr load_tree(std::string const& filename, unsigned skip_lines, operation_controller& op_ctrl, unsigned tree_size) {
  op_ctrl.update(0, tree_size);

  std::ifstream input(filename.c_str());
  input.exceptions(std::ios_base::badbit);

  while (skip_lines > 0) {
    std::string dummy_line;
    std::getline(input, dummy_line);
    --skip_lines;
  }

  std::vector<vertex_ptr> vertices;
  load_vertices(input, vertices, op_ctrl);
  try {
    load_edges(input, vertices, op_ctrl);

#ifndef NDEBUG
    // Sanity check.
    for (std::size_t index = 1; index < vertices.size(); ++index) {
      assert(vertices[index]->parents_begin() != vertices[index]->parents_end());
      assert(vertices[index]->leading_step);
    }
#endif

    op_ctrl.finished();
    if (vertices.size() > 0 && !op_ctrl.stop()) {
      assert(!vertices[0]->leading_step);  // The first vertex must be the root.
      return vertices[0];
    } else {
      return vertex_ptr();
    }
  } catch (...) {
    for (std::vector<vertex_ptr>::const_iterator vertex = vertices.begin(); vertex != vertices.end(); ++vertex) {
      delete *vertex;
    }

    throw;
  }
}

