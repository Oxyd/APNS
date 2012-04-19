/**
 * \file movement.hpp
 *
 * \brief Facilities to describe the movement of pieces on the Arimaa board.
 */

#ifndef MOVEMENT_HPP
#define MOVEMENT_HPP

#include "board.hpp"
#include "util.hpp"

#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/reverse_iterator.hpp>
#include <boost/flyweight.hpp>
#include <boost/flyweight/no_locking.hpp>
#include <boost/array.hpp>

#include <vector>
#include <utility>

namespace apns {

//! Maximum allowed number of steps in a move.
unsigned const MAX_STEPS = 4;

struct elementary_step;

namespace detail {

elementary_step el_step_from_string(std::string::const_iterator begin, std::string::const_iterator end);

}  // anonymous namespace

/**
 * Displacement of a single piece on the board or capture of a piece.
 *
 * This structure defines either the single displacement of a single piece or its capture. Elementary steps are then
 * composed together to make a full step.
 *
 * For convenience, an object of this type may only be created through the #displacement and #capture static
 * member functions. This is because \code elementary_step::displacement(from, where); \endcode explains the
 * purpose better than just \code elementary_step(from, where) \endcode which could leave the reader wondering
 * what kind of elementary step is being created.
 *
 * The field #where is only meaningful if <tt>#is_capture == false</tt>. If <tt>#is_capture == true</tt>, #where
 * contains some arbitrary, unspecified value.
 */
struct elementary_step {
  position from() const;
  direction where() const;
  bool capture() const;
  boost::optional<piece> what() const;

  std::string to_string() const;

  void set_what(boost::optional<piece> new_what);

  bool equal(elementary_step const& other) const;

  //! Construct a displacement-kind elementary step.
  static elementary_step displacement(position from, direction where, boost::optional<piece> what = boost::optional<piece>());
  //! Construct a capture-kind elementary step.
  static elementary_step make_capture(position from, boost::optional<piece> what = boost::optional<piece>());

private:
  friend elementary_step detail::el_step_from_string(std::string::const_iterator, std::string::const_iterator);

  //! A displacement.
  elementary_step(position from, direction where, boost::optional<piece> what);

  //! A capture.
  explicit elementary_step(position which, boost::optional<piece> what);

  //! Construct from string.
  explicit elementary_step(std::string::const_iterator begin, std::string::const_iterator end) {
    assert(end - begin == 4);
    assert(*begin == 'E' || *begin == 'M' || *begin == 'H' || *begin == 'D' || *begin == 'C' || *begin == 'R'
           || *begin == 'e' || *begin == 'm' || *begin == 'h' || *begin == 'd' || *begin == 'c' || *begin == 'r'
           || *begin == ' ');
    assert(*(begin + 1) >= 'a' && *(begin + 1) <= 'h');
    assert(*(begin + 2) >= '1' && *(begin + 2) <= '8');
    assert(*(begin + 3) == 'n' || *(begin + 3) == 'e' || *(begin + 3) == 's' || *(begin + 3) == 'w' || *(begin + 3) == 'x');
    std::copy(begin, end, representation_.begin());
  }

  //! Four-character representation of the elementary step. This is a string like "Rc7n" or " d3e" if the piece is not known.
  boost::array<char, 4> representation_;
};

bool operator == (elementary_step const& lhs, elementary_step const& rhs);
bool operator != (elementary_step const& lhs, elementary_step const& rhs);

inline elementary_step detail::el_step_from_string(std::string::const_iterator begin, std::string::const_iterator end) {
  return elementary_step(begin, end);
}

namespace detail {

template <typename Iter>
std::string string_from_el_steps(Iter begin, Iter end) {
  std::string result;
  result.reserve(3 * std::distance(begin, end));

  for (Iter el_step = begin; el_step != end; ++el_step) {
    if (el_step != begin)
      result += ' ';

    result += el_step->to_string();
  }

  return result;
}

} // namespace detail

class step_holder;

/**
 * A complete, valid Arimaa step.
 *
 * A full step is constructed from either one or two elementary steps. The simplest kind of step is \e displacement. A
 * displacement is merely the movement of a single game piece into an adjacent field. Other two kinds are \e push
 * and \e pull steps. These are constructed from a sequence of two elementary step, where the first elementary step
 * always defines the step that would be written first according the the official Arimaa notation rules. Thus for a
 * push step, the first elementary step is the movement of the pushee, while for a pull step, the first one is the
 * movement of the pulling piece.
 *
 * A full step also includes information about captures of pieces. Thus a full step is a complete representation of
 * the change of the game board.
 *
 * Construction of a step is done only through provided three static member functions. These functions first check
 * if such a step would be valid on the given Arimaa board and, if so, check for any captures that would result from
 * such step, store them and return the resulting step. If such a step would not be valid, these functions return nothing.
 *
 * Validity of a step is determined by Arimaa game rules. A step is not valid if, for instance, a piece would be
 * displaced beyond the dimensions of the game board, the destination field is preoccupied by another piece, or the
 * player's piece is currently frozen.
 *
 * Note, however, that these constructor functions have no way of checking for repetition of steps and as such users
 * of this class are expected to check for repetition themselves via other means.
 */
class step {
  //! A sequence of elementary steps.
  typedef std::vector<elementary_step> elementary_step_seq;

public:
  //typedef elementary_step_seq::const_iterator         el_steps_iterator;
  //typedef elementary_step_seq::const_reverse_iterator reverse_el_steps_iterator;
  
  struct el_steps_iterator : boost::iterator_facade<
    el_steps_iterator, 
    elementary_step const, 
    boost::bidirectional_traversal_tag,
    elementary_step const
  > {
    reference dereference() const { 
      return detail::el_step_from_string(position_, position_ + 4); 
    }

    void increment() { 
      position_ += 4;
      if (position_ != end_)
        ++position_;
    }

    void decrement() { 
      if (position_ == end_)
        position_ -= 4;
      else
        position_ -= 5;
    }

    bool equal(el_steps_iterator const& other) const { return position_ == other.position_; }

  private:
    friend class step;

    std::string::const_iterator position_;
    std::string::const_iterator end_;

    explicit el_steps_iterator(std::string::const_iterator pos, std::string::const_iterator end) : position_(pos), end_(end) { }
  };

  typedef boost::reverse_iterator<el_steps_iterator> reverse_el_steps_iterator;

  //! Validate and possibly construct an ordinary step.
  //! \param board The board which is to be affected by this step.
  //! \param step Movement of the single piece.
  static step_holder validate_ordinary_step(board const& board, elementary_step step);

  //! Validate and possibly construct a push kind of step.
  //! \param board The board which is to be affected by this step.
  //! \param first_step Movement of the pushed piece.
  //! \param second_step Movement of the pushing piece.
  static step_holder validate_push(board const& board,
                                   elementary_step const& first_step, elementary_step const& second_step);

  //! Validate and possibly construct a pull kind of step.
  //! \param board The board which is to be affected by this step.
  //! \param first_step Movement of the pulling piece.
  //! \param second_step Movement of the pulled piece.
  static step_holder validate_pull(board const& board,
                                   elementary_step const& first_step, elementary_step const& second_step);

  //! Construct a step from a string.
  //! \note This function does not check for the validity of the step itself. It merely checks the syntax of the input string
  //! and, if the input is syntactically valid, creates the step. This function is meant to be used primarily from the
  //! restore-search-from-disk part of the program.
  //!
  //! \returns Either the corresponding step, or nothing, if the input doesn't describe a valid step.
  static step_holder from_string(std::string const& string);

  //! Revalidate this step for a new board.
  //!
  //! \param board New board to use for this validation.
  //! \param player Player to move in this step.
  bool revalidate(board const& board, piece::color_t player) const;

  //! Does this step cause a capture on the board?
  bool capture() const;

  //! Get the beginning of the full sequence of elementary steps that represent this one step. Elementary steps in this
  //! sequence are guaranteed to have a non-empty value for the elementary_step::what member.
  el_steps_iterator step_sequence_begin() const;
  el_steps_iterator step_sequence_end() const;  //!< Get the end of the full sequence of elementary steps.

  //! Get the reverse iterator to the beginning of the reversed sequence of elementary steps.
  reverse_el_steps_iterator step_sequence_rbegin() const;
  //! Get the reverse iterator to the end of the reversed sequence of elementary steps.
  reverse_el_steps_iterator step_sequence_rend() const;

  //! How many steps does this step use? For ordinary moves, it is 1; for push and pull it's 2. Captures do not count as
  //! used steps.
  std::size_t steps_used() const;

  //! Does this step involve rabbits in any way?
  bool moves_rabbit() const;

  std::string to_string() const;

  void swap(step& other) { representation_.swap(other.representation_); }


private:
  typedef boost::flyweight<std::string, boost::flyweights::no_locking> representation_t;

  representation_t representation_;

  friend class step_holder;
  // These two are to be used by step_holder.
  step() { }
  bool empty() const { return representation_.get().empty(); }

  template <typename Iter>
  step(Iter el_steps_begin, Iter el_steps_end)
    : representation_(detail::string_from_el_steps(el_steps_begin, el_steps_end))
  { }

  //! Make a push/pull move assuming that the move is valid.
  static step make_push_pull(board const& board,
                             elementary_step first_step, elementary_step second_step);
};

bool operator == (step const& lhs, step const& rhs);
bool operator != (step const& lhs, step const& rhs);

//! Holder for a step. This mimicks boost::optional, except it doesn't require the extra bool and instead cooperates with step
//! to tell if it holds value or not.
class step_holder {
  typedef void (step_holder::* bool_type)() const;
  void this_type_does_not_support_comparisons() const { }

public:
  //! Construct an empty holder.
  step_holder() { }

  //! Implicit conversion from step, mimicking boost::optional.
  step_holder(apns::step const& step) : step_(step) { }
  
  static step_holder none;

  //! Is this holder empty?
  bool empty() { return step_.empty(); }
  bool empty() const { return step_.empty(); }
  operator bool_type() { return !empty() ? &step_holder::this_type_does_not_support_comparisons : 0; }
  operator bool_type() const { return !empty() ? &step_holder::this_type_does_not_support_comparisons : 0; }

  //! For compatibility with old code, provide an implicit conversion to optional<step>.
  operator boost::optional<apns::step> () const {
    if (!step_.empty())
      return step_;
    else
      return boost::none;
  }

  apns::step& operator * ()               { assert(!empty()); return step_; }
  apns::step const& operator * () const   { assert(!empty()); return step_; }
  apns::step* operator -> ()              { assert(!empty()); return &step_; }
  apns::step const* operator -> () const  { assert(!empty()); return &step_; }

  step_holder& operator = (apns::step const& s) {
    step_ = s;
    return *this;
  }

  apns::step* get() { if (!empty()) return &step_; else return 0; }

  void swap(step_holder& other) { step_.swap(other.step_); }

private:
  apns::step step_;
};

inline bool operator == (step_holder const& lhs, step_holder const& rhs) {
  return (lhs.empty() && rhs.empty()) || (*lhs == *rhs);
}

inline bool operator != (step_holder const& lhs, step_holder const& rhs) {
  return !operator == (lhs, rhs);
}

//! Kind of a step.
enum e_step_kind {
  ordinary,
  push,
  pull
};

//! Get the kind of a step assuming it's a given player's turn.
e_step_kind step_kind(step const& step, piece::color_t player, board const& board);

/**
 * Apply the step to the given board.
 *
 * Move pieces on the board accordingly to the description in step and remove all captured pieces.
 * \param step The step to perform.
 * \param board The board which is to be affected by this step. This must be the same board, unmodified in between,
 *              as the one passed to the used step creator function.
 */
void apply(step const& step, board& board);

//! Tro to apply the step to the given board. If the step is not applicable, returns false and doesn't modify the board.
//! Returns true if the step was applied successfuly.
bool try_apply(step const& step, board& board);

/**
 * Undo the application of a step on a board.
 *
 * This is an inverse operation to #apply. If a board was modified by an apply call, this function can be used to return the
 * board to the original state.
 *
 * \param step The step that was originally applied to the board
 * \param board The board to be reverted.
 */
void unapply(step const& step, board& board);

//! Try to unapply the step from the board. If successful, returns true. If the step can't be undone, returns false and leaves
//! the original board unmodified.
bool try_unapply(step const& step, board& board);

//! Is given piece frozen on the given board?
bool frozen(position position, board const& board);

//! Is the given piece mobile on the given board?
bool mobile(position position, board const& board);

/**
 * Iterator over an abstract sequence of all possible steps. Given a position of a piece and a board, it goes through
 * all possible steps of the specified piece.
 *
 * Internally, this iterator is implemented as a finite state machine. In each state, the machine is generating single step;
 * performing the <tt>++</tt> operation will make the machine switch to a next state. Final state of the machine is represented
 * by the singular, one-past-the-end value of the iterator, which is not dereferencable. The machine may enter the final state
 * right after creation -- for instance by specifying a frozen piece, in which case there are no possible steps for that piece.
 *
 * The state is described by the enumeration value #state as well as the two directions, #first_dir and #second_dir.
 */
class steps_iter : public boost::iterator_facade<
  steps_iter,
  step,
  boost::forward_traversal_tag,
  step
>
{
public:
  steps_iter();  //!< Construct the singular iterator.

  /**
   * Construct an iterator generating possible steps for the given piece on the given board.
   *
   * \param what_piece For what piece the steps will be generated. It must not be frozen on the board. The possition
   *                   referred to by this parameter must contain a piece.
   * \param board The board.
   */
  steps_iter(position what_piece, apns::board const& board);

private:
  friend class boost::iterator_core_access;

  apns::board const* board_;
  position piece_pos_;           //!< The specified position of the piece.
  directions_iter first_dir_;    //!< Where to move the piece (possibly pushing something else away).
  directions_iter second_dir_;   //!< Where to push the second piece (if pushing) or which of the neighbours to pull.
  step_holder result_;           //!< Resulting step.

  enum {
    ordinary,  //!< Generating ordinary steps.
    push,      //!< Generating push steps.
    pull       //!< Generating pull steps.
  } state;     //!< Current state of this iterator.

  step dereference() const;  //!< Get the resulting step.
  bool equal(steps_iter const& other) const;  //!< Compare two iterators.

  void increment();     //!< Generate next step.
  void do_increment();  //!< Try to generate a next step -- possibly letting result be empty.

  void advance_first();  //!< Advance first_dir.

  void advance_second_to_empty();   //!< Make #second_dir point to a nonempty position at or after current value of #second_dir
  void advance_second_to_weaker();  //!< Make #second_dir point to a weaker opponent piece at or after the current value

  void generate_ordinary();         //!< Generate an ordinary step.
  void generate_push();             //!< Generate a push step.
  void generate_ordinary_or_push(); //!< Generate either a push or ordinary step.
};

//! Return the start of the abstract sequence of all possible steps for the given piece.
steps_iter steps_begin(position what, board const& board);
steps_iter steps_end();  //!< Return the end of the sequence returned by steps_begin.

/**
 * An iterator generating all possible steps for a given player. This iterator basically just goes through all mobile pieces
 * of the specified player and generates steps using #steps_iter.
 */
class all_steps_iter : public boost::iterator_facade<
  all_steps_iter,
  step,
  boost::forward_traversal_tag,
  step
>
{
public:
  all_steps_iter();
  all_steps_iter(apns::board const& board, piece::color_t player);

private:
  friend class boost::iterator_core_access;

  apns::board const*            board_;
  apns::board::pieces_iterator  current_piece_;
  steps_iter                    current_step_;
  piece::color_t                player_;

  void increment();
  reference dereference() const;
  bool equal(all_steps_iter const& other) const;

  void forward();
  void forward_to_mobile();
};

all_steps_iter all_steps_begin(board const& board, piece::color_t player);
all_steps_iter all_steps_end();

} // namespace apns

namespace std {
  template <> inline void swap(apns::step& x, apns::step& y) { x.swap(y); }
  template <> inline void swap(apns::step_holder& x, apns::step_holder& y) { x.swap(y); } 
}

#endif
