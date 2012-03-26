#include "util.hpp"
#include "tree.hpp"

#include <boost/python.hpp>

namespace {

/**
 * Python wrapper for operation_controller. Required to be able to inherit from it in Python and override virtual functions.
 */
class operation_controller_wrapper : public apns::operation_controller, public boost::python::wrapper<apns::operation_controller> {
  virtual void do_update() {
    get_override("doUpdate")();
  }
public:
  explicit operation_controller_wrapper(unsigned update_interval)
    : operation_controller(update_interval)
  { }
};

} // anonymous namespace

//! Export functions and types declared in util.hpp
void export_util() {
  using namespace boost::python;

  class_<operation_controller_wrapper, boost::noncopyable>("OperationController",
      "Controller for long-running operations",
      init<unsigned>())
      .def("update", static_cast<void (apns::operation_controller::*)()>(&apns::operation_controller::update),
          "Update the controller and, possibly, the interface")
      .def("update", static_cast<void (apns::operation_controller::*)(unsigned, unsigned)>(&apns::operation_controller::update),
          "Update the controller, providing information about the progress")
      .def("requestStop", &apns::operation_controller::request_stop,
          "Request that the algorithm stop")
      .add_property("stop", &apns::operation_controller::stop, "Should the algorithm stop now?")
      .add_property("workDone", &apns::operation_controller::get_work_done, "Get the amount of work done")
      .add_property("workTotal", &apns::operation_controller::get_work_total, "Get the amount of work total")

      .def("doUpdate", pure_virtual(&apns::operation_controller::do_update))
      ;
}

