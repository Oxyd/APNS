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

} // anonymous namespace

namespace apns {

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

} // namespace apns

