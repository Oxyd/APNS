#include "util.hpp"
#include "search.hpp"

#include <iostream>
#include <fstream>
#include <sstream>
#include <stack>
#include <stdexcept>

namespace {

/**
 * Dump the vertices of the three rooted at 'root' into output. This function will assign each vertex of the tree a unique
  positive integer and set its value as vertex.pickle_number. The root of the tree is guaranteed to be assigned pickle_number
  of 1.
 */
void dump_vertices(std::ostream& output, vertex_ptr root, operation_controller& op_ctrl) {
  std::stack<vertex_ptr> stack;
  stack.push(root);
  unsigned number = 1;

  while (!stack.empty() && !op_ctrl.stop()) {
    vertex_ptr vertex = stack.top();
    stack.pop();

    if (vertex->pickle_number == 0) {
      // This vertex hasn't yet been pickled.

      vertex->pickle_number = number++;

      std::string const step = vertex->leading_step ? vertex->leading_step->to_string() : "root";
      std::string const type = vertex->type == vertex::type_and ? "and" : "or";

      output << vertex->pickle_number << ": " << step << ": " << type << " " << vertex->steps_remaining << " ";
      if (vertex->proof_number < vertex::infty) output << vertex->proof_number;
      else                                      output << "infty";
      output << ' ';
      if (vertex->disproof_number < vertex::infty) output << vertex->disproof_number;
      else                                         output << "infty";
      output << '\n';

      for (vertex::vertex_list::const_iterator child = vertex->children_begin(); child != vertex->children_end(); ++child) {
        stack.push(*child);
      }

      op_ctrl.update(op_ctrl.get_work_done() + 1, op_ctrl.get_work_total());
    }
  }
}

/**
 * Dump the edges of the tree rooted at 'root' into output. This function will set pickled of each vertex back to true.
 */
void dump_edges(std::ostream& output, vertex_ptr root, operation_controller& op_ctrl) {
  std::stack<vertex_ptr> stack;
  stack.push(root);

  while (!stack.empty() && !op_ctrl.stop()) {
    vertex_ptr vertex = stack.top();
    stack.pop();

    if (!vertex->pickled) {
      if (vertex->children_begin() != vertex->children_end()) {  // Only actually dump the vertex if it has any children.
        output << vertex->pickle_number << " : ";

        for (vertex::vertex_list::const_iterator child = vertex->children_begin(); child != vertex->children_end(); ++child) {
          output << (*child)->pickle_number << ' ';
          stack.push(*child);
        }
        output << '\n';
      }

      vertex->pickled = true;
      op_ctrl.update(op_ctrl.get_work_done() + 1, op_ctrl.get_work_total());
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
 * pickle_number == index - 1. The returned vertices have pickle_number set to 0.
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

      vertex_ptr vertex = vertex::create();

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

      vertices.push_back(vertex);
      op_ctrl.update(op_ctrl.get_work_done() + 1, op_ctrl.get_work_total());
    } else {
      break;
    }
  }
}

//! Load edges from input and store them in vertices. vertices is expected to be a list as returned by _loadVertices.
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
    while (parser >> edge) {
      vertex->add_child(vertices[edge - 1]);
    }

    op_ctrl.update(op_ctrl.get_work_done() + 1, op_ctrl.get_work_total());
  }
}

}

prng_t prng;

char* counting_allocator::malloc(size_type bytes) {
  allocated += bytes;
  char* storage = new (std::nothrow) char [bytes + sizeof(size_type)];
  if (storage) {
    *reinterpret_cast<size_type*>(storage) = bytes + sizeof(size_type);
    return storage + sizeof(size_type);
  } else {
    return 0;
  }
}

void counting_allocator::free(char const* block) {
  if (block) {
    char const* real_block = block - sizeof(size_type);
    size_type alloc_size = *reinterpret_cast<size_type const*>(real_block);
    allocated -= alloc_size;
    delete [] real_block;
  }
}

counting_allocator::size_type counting_allocator::allocated_total() {
  return allocated;
}

counting_allocator::size_type counting_allocator::allocated = 0;

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

  op_ctrl.update(0, 2 * tree_size);  // The tree needs to be traversed twice.

  dump_vertices(output, root, op_ctrl);
  output << VERTICES_EDGES_SEPARATOR << '\n';
  dump_edges(output, root, op_ctrl);

  op_ctrl.finished();
}

vertex_ptr load_tree(std::string const& filename, unsigned skip_lines, operation_controller& op_ctrl, unsigned tree_size) {
  op_ctrl.update(0, 2 * tree_size);

  std::ifstream input(filename.c_str());
  input.exceptions(std::ios_base::badbit);

  while (skip_lines > 0) {
    std::string dummy_line;
    std::getline(input, dummy_line);
    --skip_lines;
  }

  std::vector<vertex_ptr> vertices;
  load_vertices(input, vertices, op_ctrl);
  load_edges(input, vertices, op_ctrl);

  op_ctrl.finished();
  if (vertices.size() > 0 && !op_ctrl.stop()) {
    assert(!vertices[0]->leading_step);  // The first vertex must be the root.
    return vertices[0];
  } else {
    return vertex_ptr();
  }
}
