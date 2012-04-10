#include "util.hpp"

namespace apns {

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

