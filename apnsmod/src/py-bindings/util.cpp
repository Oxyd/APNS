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

struct log_sink_wrap : apns::log_sink, boost::python::wrapper<apns::log_sink> {
  virtual bool null() const {
    using namespace boost::python;

    if (override f = this->get_override("null"))
      return f();
    else
      return log_sink::null();
  }

  bool default_null() const { return log_sink::null(); }

  virtual void do_put(std::stringstream const& data) {
    this->get_override("doPut")(data.str());
  }

  virtual void do_flush() {
    using namespace boost::python;

    if (override f = this->get_override("doFlush"))
      f();
  }

  void default_flush() { }
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

  class_<log_sink_wrap, boost::noncopyable>(
    "LogSink",
    "An abstract base-class for defining own log sinks."
  )
    .def("put", &apns::log_sink::put<std::string>,
         "s.put(str) -> None\n\n"
         "Put something into the sink.")
    .def("flush", &apns::log_sink::flush,
         "s.flush() -> None\n\n"
         "Flush this sink.")
    .def("null", &apns::log_sink::null, &log_sink_wrap::default_null,
         "s.null() -> Bool\n\n"
         "Is this sink a null sink?")
    .def("doPut", pure_virtual(&log_sink_wrap::do_put),
         "s.doPut(str) -> None\n\n"
         "Do the work of putting data into the sink.")
    .def("doFlush", &log_sink_wrap::do_flush,
         "s.doFlush() -> None\n\n"
         "Do the work of flushing the sink.")
    ;

  class_<apns::stdout_sink, bases<apns::log_sink>, boost::noncopyable>(
    "StdoutSink", "A sink that streams data into the standard output."
  );

  class_<apns::file_sink, bases<apns::log_sink>, boost::noncopyable>(
    "FileSink", "Sink that streams data into a file",
    init<std::string const&>()
  );

  class_<apns::null_sink, bases<apns::log_sink>, boost::noncopyable>(
    "NullSink", "Sink that does nothing."
  );
}

