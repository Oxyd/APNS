#include "util.hpp"
#include "tree.hpp"

#include <iostream>
#include <fstream>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <cstdlib>
#include <map>

#include <boost/lexical_cast.hpp>

namespace {

//! Throw a "Loading Tree Failed" error.
void throw_loading_failed() {
  throw std::runtime_error("Loading the search tree failed: Specified file is incorrect or corrupt");
}

std::size_t memory_usage = 0;                     //!< Total memory usage of this module.

}

#if 0
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

#endif

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

#if 0
void dump_tree(std::string const& filename, vertex_ptr root, bool append, operation_controller& op_ctrl, unsigned tree_size) {
  std::ofstream output(filename.c_str(), append ? std::ios_base::out | std::ios_base::app : std::ios_base::out);
  output.exceptions(std::ios_base::badbit | std::ios_base::failbit);

  op_ctrl.update(0, tree_size);

  std::stack<vertex const*> stack;
  stack.push(root);

  while (!stack.empty() && !op_ctrl.stop()) {
    vertex const* v = stack.top();
    stack.pop();

    std::string const step = v->leading_step ? v->leading_step->to_string() : "root";
    std::string const type = v->type == vertex::type_and ? "and" : "or";

    output << step << " : " << type << ' ' << v->steps_remaining << ' '
           << (v->proof_number < vertex::infty ? boost::lexical_cast<std::string>(v->proof_number) : std::string("infty"))
           << ' '
           << (v->disproof_number < vertex::infty ? boost::lexical_cast<std::string>(v->disproof_number) : std::string("infty"))
           << ' '
           << std::distance(v->children_begin(), v->children_end())
           << '\n';

    for (vertex::const_reverse_children_iterator child = v->children_rbegin(); child != v->children_rend(); ++child) {
      stack.push(&*child);
    }

    op_ctrl.update(op_ctrl.get_work_done() + 1, op_ctrl.get_work_total());
  }

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

  std::auto_ptr<vertex> root(new vertex);

  std::stack<std::pair<vertex*, vertex*> > stack;  // Stack of (current child iterator, one-past-the-end child iterator).
  stack.push(std::make_pair(root.get(), root.get() + 1));

  while (!stack.empty() && !op_ctrl.stop()) {
    vertex* v = stack.top().first;
    if (v == stack.top().second) throw_loading_failed();

    ++stack.top().first;
    if (stack.top().first == stack.top().second) {
      stack.pop();
    }

    std::string step_str;
    std::string read;

    input >> read;
    while (input && read != ":") {
      if (!step_str.empty()) {
        step_str += ' ';
      }
      step_str += read;
      input >> read;
    }

    if (!input) throw_loading_failed();

    if (step_str != "root") {
      boost::optional<step> maybe_step = step::from_string(step_str);
      if (!maybe_step) throw_loading_failed();
      v->leading_step = *maybe_step;
    }

    std::string type_str;
    input >> type_str;
    if (!input) throw_loading_failed();

    if (type_str == "and") {
      v->type = vertex::type_and;
    } else if (type_str == "or") {
      v->type = vertex::type_or;
    } else {
      throw_loading_failed();
    }

    input >> v->steps_remaining;
    if (!input) throw_loading_failed();

    std::string num;
    input >> num;
    if (!input) throw_loading_failed();

    if (num == "infty") {
      v->proof_number = vertex::infty;
    } else {
      try {
        v->proof_number = boost::lexical_cast<vertex::number_t>(num);
      } catch (boost::bad_lexical_cast&) {
        throw_loading_failed();
      }
    }

    input >> num;
    if (!input) throw_loading_failed();

    if (num == "infty") {
      v->disproof_number = vertex::infty;
    } else {
      try {
        v->disproof_number = boost::lexical_cast<vertex::number_t>(num);
      } catch (boost::bad_lexical_cast&) {
        throw_loading_failed();
      }
    }

    unsigned children_count;
    input >> children_count;
    if (!input) throw_loading_failed();

    if (children_count > 0) {
      v->alloc_children(children_count);
      stack.push(std::make_pair(v->children_begin(), v->children_end()));

      for (vertex::children_iterator child = v->children_begin(); child != v->children_end(); ++child) {
        child->set_parent(v);
      }
    }

    op_ctrl.update(op_ctrl.get_work_done() + 1, op_ctrl.get_work_total());
  }

  return root.release();
}
#endif

