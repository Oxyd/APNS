#include "util.hpp"
#include "tree.hpp"

#include <boost/python.hpp>

namespace {

/**
 * Python wrapper for operation_controller. Required to be able to inherit from it in Python and override virtual functions.
 */
class operation_controller_wrapper : public operation_controller, public boost::python::wrapper<operation_controller> {
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

  def("memoryUsedTotal", &get_memory_usage,
      "Get the amount of total memory used for search in bytes.");

  class_<operation_controller_wrapper, boost::noncopyable>("OperationController",
      "Controller for long-running operations",
      init<unsigned>())
      .def("update", static_cast<void (operation_controller::*)()>(&operation_controller::update),
          "Update the controller and, possibly, the interface")
      .def("update", static_cast<void (operation_controller::*)(unsigned, unsigned)>(&operation_controller::update),
          "Update the controller, providing information about the progress")
      .def("requestStop", &operation_controller::request_stop,
          "Request that the algorithm stop")
      .add_property("stop", &operation_controller::stop, "Should the algorithm stop now?")
      .add_property("workDone", &operation_controller::get_work_done, "Get the amount of work done")
      .add_property("workTotal", &operation_controller::get_work_total, "Get the amount of work total")

      .def("doUpdate", pure_virtual(&operation_controller::do_update))
      ;

  //def("dumpTree", &dump_tree);
  //def("loadTree", &load_tree, return_value_policy<manage_new_object>());
}

