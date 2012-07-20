#ifndef UTIL_HPP
#define UTIL_HPP

#include <boost/utility.hpp>
#include <boost/function.hpp>
#include <boost/timer/timer.hpp>

#include <iostream>
#include <sstream>
#include <fstream>

void export_util();

namespace apns {

/**
 * Controller for long-running operations. It serves as a simple interface
 * between the algorithm and the user interface.  This class periodically calls
 * a given callback which can be used to refresh the user interface with
 * progress information.  It also allows algorithms to provide information
 * about the progress and lets the user interface signal the algorithm that the
 * operation should be terminated.
 *
 * It is an abstract base class. User interfaces are expected to inherit this
 * class.
 */
class operation_controller : boost::noncopyable {
public:
  /**
   * Construt a controller.
   * \param ms_update_time How often, in milliseconds, should #update() be
   *   called.
   */
  explicit operation_controller(unsigned ms_update_time = 100);

  virtual ~operation_controller();

  /**
   * Update the controller. This function should be called often by the running
   * algorithm (e.g. once each iteration of the main algorithm loop).
   */
  void update();

  /**
   * Update the controller and provide information about progress. Just as the
   * previous version of update, this function should be called often by the
   * algorithm.
   *
   * \param done How much work out of #work_total is done?
   * \param work_total How much work there is in total?
   */
  void update(unsigned done, unsigned work_total);

  /**
   * Tell the interface that the job is finished. This is the same as update()
   * except that it always notifies the user interface.
   */
  void finished();

  /**
   * Request that the algorithm stops.
   * \note The algorithm will only stop after the control has returned from
   *   #do_update.
   */
  void request_stop();

  /**
   * Should the algorithm stop? If this value is true, the algorithm should
   * stop doing its work because the user has requested that the operation be
   * cancelled.
   */
  bool stop() const;

  //! Get the amount of work done.
  unsigned get_work_done() const;

  //! Get the amount of total work. If the returned value is 0, the algorithm
  //! hasn't provided any information about its progress.
  unsigned get_work_total() const;

private:
  // So that the Python interface has access to do_update even though it's 
  // private.
  friend void ::export_util();  

  //! Update the user interface. This function will be called each 
  //! #ms_update_time milliseconds.
  virtual void do_update() = 0;

  unsigned                ms_update_time_;
  bool                    stop_requested_;
  unsigned                work_done_;
  unsigned                work_total_;
  boost::timer::cpu_timer update_timer_;
};

/**
 * Convenience class for implementing an #operation_controller. This class
 * simply does nothing. Useful if you want to call a function that requires an
 * #operation_controller, but you don't actually care about the progress.
 */
class null_operation_controller : public operation_controller {
public:
  null_operation_controller() : 
    operation_controller(60000) // Call #do_update only once a minute, so that 
                                // things don't get slowed down too much.
  { }

private:
  virtual void do_update() { }
};

//! Helper to provide the Strong Guarantee. Upon destruction of this object,
//! unless .commit() has been called, the rollback function shall be called.
struct transaction {
  explicit transaction(boost::function<void ()> const& rollback) 
    : rollback_(rollback)
    , commited_(false) 
  { }

  //! Convenience constructor -- calls perform() once before any
  //initialisation. This is to allow constructions like 
  //! transaction t(&do_something, &guard); instead of 
  //! do_something(); transaction t(&guard);.
  transaction(boost::function<void ()> const& perform,
              boost::function<void ()> const& rollback) 
    : rollback_(rollback)
    , commited_(false) 
  {
    perform();  // If this throws, the destructor won't be called as 
  }             // transaction has not begun its lifetime yet.

  ~transaction()  { if (!commited_) rollback_(); }
  void commit()   { commited_ = true; }

private:
  boost::function<void ()>  rollback_;
  bool                      commited_;
};

//! Polymorphic abstract base-class defining the logging interface.
class log_sink {
public:
  virtual ~log_sink() { }

  //! Stream something into the sink.
  template <typename T>
  void put(T const& t) {
    if (!null()) {
      std::stringstream data;
      data << t;
      do_put(data);
    }
  }

  //! Flush the stream.
  void flush() { do_flush(); }

  //! Is the sink null?
  virtual bool null() const { return false; }

private:
  //! Put some actual data into the sink.
  virtual void do_put(std::stringstream const& data) = 0;

  //! Really flush the stream.
  virtual void do_flush() { }
};

//! Log inserter operator.
template <typename T>
log_sink& operator << (log_sink& log, T const& t) {
  log.put(t);
  return log;
}

//! A log sink for printing to the standard output.
class stdout_sink : public log_sink {
  virtual void do_put(std::stringstream const& data) {
    std::cout << data.rdbuf();
  }
};

//! A long sink for printing into a file.
class file_sink : public log_sink {
public:
  //! \throws std::ios_base::failure if the log file could not be opened.
  explicit file_sink(std::string const& filename);

private:
  std::ofstream out_;

  virtual void do_put(std::stringstream const& data);
  virtual void do_flush();
};

//! A log sink that does nothing.
class null_sink : public log_sink {
public:
  virtual bool null() const { return true; }

private:
  virtual void do_put(std::stringstream const&) { }
};

} // namespace apns

#endif

