#ifndef UTIL_HPP
#define UTIL_HPP

#include <boost/random/mersenne_twister.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/utility.hpp>
#include <boost/timer.hpp>

typedef boost::mt19937 prng_t;

extern prng_t prng;

//! Get the total memory usage by this module.
std::size_t get_memory_usage();

/**
 * Controller for long-running operations. It serves as a simple interface between the algorithm and the user interface.
 * This class periodically calls a given callback which can be used to refresh the user interface with progress information.
 * It also allows algorithms to provide information about the progress and lets the user interface signal the algorithm that
 * the operation should be terminated.
 *
 * It is an abstract base class. User interfaces are expected to inherit this class.
 */
class operation_controller : boost::noncopyable {
public:
  /**
   * Construt a controller.
   * \param ms_update_time How often, in milliseconds, should #update() be called.
   */
  explicit operation_controller(unsigned ms_update_time = 100);

  virtual ~operation_controller();

  /**
   * Update the controller. This function should be called often by the running algorithm (e.g. once each iteration of the
   * main algorithm loop).
   */
  void update();

  /**
   * Update the controller and provide information about progress. Just as the previous version of update, this function should
   * be called often by the algorithm.
   * \param done How much work out of #work_total is done?
   * \param work_total How much work there is in total?
   */
  void update(unsigned done, unsigned work_total);

  /**
   * Tell the interface that the job is finished. This is the same as update() except that it always notifies the user interface.
   */
  void finished();

  /**
   * Request that the algorithm stops.
   * \note The algorithm will only stop after the control has returned from #do_update.
   */
  void request_stop();

  /**
   * Should the algorithm stop? If this value is true, the algorithm should stop doing its work because the user has requested
   * that the operation be cancelled.
   */
  bool stop() const;

  //! Get the amount of work done.
  unsigned get_work_done() const;

  //! Get the amount of total work. If the returned value is 0, the algorithm hasn't provided any information about its progress.
  unsigned get_work_total() const;

private:
  friend void export_util();  // So that the Python interface has access to do_update even though it's private.

  //! Update the user interface. This function will be called each #ms_update_time milliseconds.
  virtual void do_update() = 0;

  unsigned      ms_update_time;
  bool          stop_requested;
  unsigned      work_done;
  unsigned      work_total;
  boost::timer  update_timer;
};

/**
 * Convenience class for implementing an #operation_controller. This class simply does nothing. Useful if you want to call a
 * function that requires an #operation_controller, but you don't actually care about the progress.
 */
class null_operation_controller : public operation_controller {
public:
  null_operation_controller() : operation_controller(60000) { }  // Call #do_update only once a minute, so that things don't
                                                                 // get slowed down too much.
private:
  virtual void do_update() { }
};

struct vertex;
typedef boost::shared_ptr<vertex> vertex_ptr;

/**
 * Dump the search tree into a file specified by filename.
 * \param filename Name of file to write the output to.
 * \param root Root of the tree.
 * \param append If true, the tree will be appended to the file; the file will be overwritten otherwise.
 * \param op_ctrl Operation controller for keeping the interface updated about the progress.
 * \param tree_size Number of vertices in the tree.
 */
void dump_tree(std::string const& filename, vertex_ptr root, bool append, operation_controller& op_ctrl,
    unsigned tree_size);

/**
 * Load the search tree from a specified file.
 * \param filename Name of the file to read.
 * \param skip_lines How many lines to skip from the start of the file?
 * \param op_ctrl Operation controller for keeping the interface updated about the progress.
 * \param tree_size Expected number of vertices in the tree.
 */
vertex_ptr load_tree(std::string const& filename, unsigned skip_lines, operation_controller& op_ctrl, unsigned tree_size);

#endif

