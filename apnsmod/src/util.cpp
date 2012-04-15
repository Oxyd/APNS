#include "util.hpp"

namespace apns {

operation_controller::operation_controller(unsigned ms_update_time)
  : ms_update_time_(ms_update_time)
  , stop_requested_(false)
  , work_done_(0)
  , work_total_(0)
{ }

operation_controller::~operation_controller() { }

void operation_controller::update() {
  double const MS = 1000.0;

  if (update_timer_.elapsed() >= ms_update_time_ / MS) {
    do_update();
    update_timer_.restart();
  }
}

void operation_controller::update(unsigned done, unsigned total) {
  work_done_ = done;
  work_total_ = total;
  update();
}

void operation_controller::finished() {
  do_update();
}

bool operation_controller::stop() const {
  return stop_requested_;
}

void operation_controller::request_stop() {
  stop_requested_ = true;
}

unsigned operation_controller::get_work_done() const {
  return work_done_;
}

unsigned operation_controller::get_work_total() const {
  return work_total_;
}

} // namespace apns

