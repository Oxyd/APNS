#ifndef TREE_HPP
#define TREE_HPP

#include "board.hpp"
#include "movement.hpp"
#include "util.hpp"

#include <boost/optional.hpp>
#include <boost/utility.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/cstdint.hpp>
#include <boost/integer_traits.hpp>
#include <boost/ref.hpp>
#include <boost/iterator/iterator_adaptor.hpp>
#include <boost/iterator/reverse_iterator.hpp>
#include <boost/type_traits.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/lambda/lambda.hpp>

#include <cstddef>
#include <cassert>
#include <iterator>
#include <stack>

namespace apns {

/** A vertex of the game tree.
 *
 * Vertices are non-copyable to prevent costly copies of large subtrees.
 * Internally, they are implemented via resizeable array -- as such, their
 * allocated size may be larger than the number of children. The pack() member
 * function can be used to make the vertex only take up as much free-store
 * space as required.
 *
 * \note This class behaves like a container of children. The children are
 * always sorted in the order in which they were inserted.
 *
 * \note Operations such as add_child, remove_child, reserve, resize and pack
 * may and will invalidate all iterators, pointers and references to any
 * children of this vertex.
 *
 * \note This could probably have been designed better. Oh well...
 */
class vertex : boost::noncopyable {
  typedef boost::ptr_vector<vertex> children_container;

  //! This is to provide the op ->* that is for some reason missing in 
  //! ptr_vector's iterator...
  template <typename Iter>
  struct children_iterator_base : boost::iterator_adaptor<
    children_iterator_base<Iter>,
    Iter
  > {
    children_iterator_base() { }

    // Intentionally implicit.
    children_iterator_base(Iter i) 
      : children_iterator_base::iterator_adaptor_(i) 
    { }

    template <typename T>
    T& operator ->* (T vertex::*mptr) { return (*(this->base())).*mptr; }
  };

public:
  //! Type of the proof and disproof numbers.
  typedef boost::uint32_t number_t;  
  
  typedef children_iterator_base<children_container::iterator> 
    children_iterator;
  typedef children_iterator_base<children_container::const_iterator> 
    const_children_iterator;
  typedef boost::reverse_iterator<children_iterator> 
    reverse_children_iterator;
  typedef boost::reverse_iterator<const_children_iterator>
    const_reverse_children_iterator;

  //! Maximum value of a proof- or disproof number.
  static number_t const max_num = boost::integer_traits<number_t>::const_max;

  //! Infinity value used in the algorithm.
  static number_t const infty = max_num - 1;
  
  //! Type of this node.
  enum e_type {
    type_and,   //!< Defender's turn.
    type_or     //!< Attacker's turn.
  };

  number_t    proof_number;
  number_t    disproof_number;
  step_holder step;
  int         steps_remaining;
  e_type      type;

  vertex();
  ~vertex();

  //! Add a child at the end of the children list and return an iterator to it.
  //! May invalidate all existing iterators to children of this vertex.
  children_iterator add_child();

  //! Remove child specified by an iterator into this vertex. May invalidate 
  //! all existing iterators to children of this vertex.
  //! \returns Iterator to the child following the removed one.
  children_iterator remove_child(children_iterator child);
  
  children_iterator       children_begin()        { return children_.begin(); }
  children_iterator       children_end()          { return children_.end(); }
  const_children_iterator children_begin() const  { return children_.begin(); }
  const_children_iterator children_end() const    { return children_.end(); }

  reverse_children_iterator children_rbegin() {
    return reverse_children_iterator(children_end());
  }

  reverse_children_iterator children_rend() {
    return reverse_children_iterator(children_begin());
  }

  const_reverse_children_iterator children_rbegin() const {
    return const_reverse_children_iterator(children_end());
  }

  const_reverse_children_iterator children_rend() const {
    return const_reverse_children_iterator(children_begin());
  }

  std::size_t children_count() const { return children_.size(); }
  bool leaf() const { return children_.empty(); }

  //! Construct an iterator to a child from a pointer to a child of this 
  //! vertex. The behaviour is undefined if child does not point to a child of 
  //! this vertex.
  //!
  //! \note This is surprisingly ineffective (O(n) instead of O(1)) -- could
  //!   it be improved or removed?
  children_iterator iter_from_ptr(vertex* child) {
    using namespace boost::lambda;
    return std::find_if(children_.begin(), children_.end(),
                        &_1 == child);
  }

  //! Sort the children of this vertex according to a comparator.
  template <typename Compare>
  void sort_children(Compare comp) {
    std::sort(children_.base().begin(), children_.base().end(),
              ptr_compare<Compare>(comp));
  }

  //! Swap two children of this vertex.
  void swap_children(children_iterator first, children_iterator second) {
    assert(first != children_end());
    assert(second != children_end());

    if (first != second)
      std::swap(*first.base().base(), *second.base().base());
  }

  //! Transfer a child from another vertex to this one. The child is removed 
  //! from the source vertex. Insertion into this one is done at the end by 
  //! default, but that may be changed by specifying the before parameter.
  //!
  //! \note As the source vertex's size is reduced, it may be advantageous to 
  //!   .pack() it after this operation.
  void transfer_child(vertex& other, vertex::children_iterator child,
                      vertex::children_iterator before) {
    children_.transfer(before.base(), child.base(), other.children_);
  }

  void transfer_child(vertex& other, vertex::children_iterator child) {
    transfer_child(other, child, children_end());
  }

  //! Make this vertex allocate enough memory to hold new_size children, but 
  //! don't really create the children yet. This may invalidate all existing 
  //! iterators to children of this vertex.
  void reserve(std::size_t new_size);

  //! Make this vertex have new_size children. If new_size is more than the 
  //! current size, new children will be  default-constructed at the end of the 
  //! children sequence. If new_size is less than the current size, this is 
  //! equivalent to removing children from the end until this vertex has 
  //! exactly new_size children.
  //!
  //! This function may invalidate all existing iterators to children of this 
  //! vertex.
  //!
  //! \note This doesn't necessarily free up any memory if the size is reduced.
  void resize(std::size_t new_size);

  //! Make this vertex use only as much memory as required to hold all its 
  //! children. This may invalidate all existing iterator
  //! to children of this vertex.
  void pack();

  //! Get an aproximate total memory usage by all vertices of all trees.
  static std::size_t alloc_size() { return alloc_; }
  
private:
  static std::size_t alloc_;

  //! Compare void*s that point to vertices according to a given comparator.
  template <typename Compare>
  struct ptr_compare {
    explicit ptr_compare(Compare comp) : comp_(comp) { }

    bool operator () (void* lhs, void* rhs) {
      return comp_(*static_cast<vertex*>(lhs), *static_cast<vertex*>(rhs));
    }

  private:
    Compare comp_;
  };

  children_container children_;
};

//! Given a vertex type, return its opposite.
inline vertex::e_type opposite_type(vertex::e_type t) {
  return t == vertex::type_or ? vertex::type_and : vertex::type_or;
}

namespace detail {

template <typename Compare>
struct ptr_compare {
  explicit ptr_compare(Compare comp = Compare()) : comp_(comp) { }
  bool operator () (vertex const* lhs, vertex const* rhs) {
    return comp_(*lhs, *rhs);
  }

private:
  Compare comp_;
};

} // namespace detail

//! Sort the children of a vertex according to a comparator.
template <typename Compare>
void sort_children(vertex& parent, Compare comp = Compare()) {
  parent.sort_children(comp);
}

//! Update the order of children of a vertex assuming it was sorted previously 
//! but child is now out of order.
//!
//! \returns Iterator to the given child in its new position.
template <typename Compare>
vertex::children_iterator
resort_children(vertex& parent, vertex::children_iterator child, 
                Compare comp = Compare()) {
  assert(child != parent.children_end());

  // So long as the child is greater than its sibling to the right, bubble it 
  // right.
  while (boost::next(child) != parent.children_end() && 
         comp(*boost::next(child), *child)) {
    vertex::children_iterator equal_range_end = boost::next(child);
    while (equal_range_end != parent.children_end() &&
           boost::next(equal_range_end) != parent.children_end() &&
           !comp(*equal_range_end, *boost::next(equal_range_end)))
      ++equal_range_end;

    parent.swap_children(child, equal_range_end);
    child = equal_range_end;
  }

  // And conversly, so long as the sibling to the left is greater or equal than 
  // the child, bubble left. Note that this won't do anything if the previous 
  // loop already re-sorted the children.
  while (child != parent.children_begin() && 
         !comp(*boost::prior(child), *child)) {
    vertex::children_iterator equal_range_begin = boost::prior(child);
    while (equal_range_begin != parent.children_begin() &&
           !comp(*boost::prior(equal_range_begin), *equal_range_begin))
      --equal_range_begin;

    parent.swap_children(child, equal_range_begin);
    child = equal_range_begin;
  }

  return child;
}

//! Transfer a child from one vertex to another.
//! \param source The source vertex.
//! \param child An iterator into source specifying the child to transfer.
//! \param dest Destination vertex.
//! \param before Iterator into dest specifying before which position is the 
//!   insertion to take place.
inline void transfer_child(vertex& source, vertex::children_iterator child,
                           vertex& dest, vertex::children_iterator before) {
  dest.transfer_child(source, child, before);
}

inline void transfer_child(vertex& source, vertex::children_iterator child,
                           vertex& dest) {
  dest.transfer_child(source, child);
}

//! A container for the game and its search tree.
class game : boost::noncopyable {
public:
  vertex          root;           //!< Root of the search tree.
  piece::color_t  attacker;       //!< Attacking player's colour.
  board           initial_state;  //!< Initial board.

  game(board const& initial_state, piece::color_t attacker) :
    attacker(attacker),
    initial_state(initial_state)
  {
    root.type             = vertex::type_or;
    root.proof_number     = 1;
    root.disproof_number  = 1;
    root.steps_remaining  = 4;
  }
};

//! Save a game object to a file.
//!
//! \param game The game object to be saved.
//! \param filename Name of the file to save to. This will be opened by an 
//!   ofstream.
//! \param op_ctrl OperationController for this operation. This algorithm does 
//!   not provide information about its progress.
//!
//! \throws std::runtime_error Thrown if the file could not be written 
//!   successfully.
void save_game(boost::shared_ptr<game> const& game,
               std::string const& filename, operation_controller& op_ctrl);
inline void save_game(boost::shared_ptr<game> const& game,
                      std::string const& filename) {
  null_operation_controller op_ctrl;
  save_game(game, filename, op_ctrl);
}

//! Load a game from a file.
//!
//! \param filename File to be read. This will be opened by an ifstream.
//! \param op_ctrl OperationController for this operation. This algorithm does 
//!   not provide information about its progress.
//!
//! \returns A new Game instance and total number of vertices in the tree.
//!
//! \throws std::runtime_error Thrown if the file could not be read 
//!   successfully, or if its format is not valid.
std::pair<boost::shared_ptr<game>, std::size_t>
load_game(std::string const& filename, operation_controller& op_ctrl);

inline std::pair<boost::shared_ptr<game>, std::size_t> 
load_game(std::string const& filename) {
  null_operation_controller op_ctrl;
  return load_game(filename, op_ctrl);
}

//! A no-op visitor for traverser.
struct null_visitor {
  void operator () (vertex const&) { }
};

//! A stop condition that always returns false.
struct null_stop_condition {
  bool operator () (vertex const&) { return false; }
};

//! Convenience typedef so that users don't have to type out the function 
//! signature each time they want to use a traverser with an ordinary function.
typedef vertex const* (*fun_tp)(vertex const&);

/** Generic tree traverser. This traverser doesn't have to visit every node in
 * the tree; instead, the path it will take is dictated by the specified
 * TraversalPolicy.
 *
 * \tparam CVVertex Possibly CV-qualified vertex.
 * \tparam TraversalPolicy Specifies how the tree will be traversed. It is a
 *   functor taking a vertex& and returning a pointer to the desired successor,
 *   or null if the traversal is to be stopped.
 * \tparam Visitor Will be called on each node that is traversed.
 * \tparam StopCondition Will be called on each node that is traversed (after
 *   Visitor) and if it returns true, the traversal
 *   will be stopped and the last visited vertex returned.
 */
template <
  typename CVVertex,
  typename TraversalPolicy,
  typename Visitor,
  typename StopCondition
>
CVVertex* traverse(CVVertex& root, TraversalPolicy traversal_policy,
                 Visitor visitor, StopCondition stop_condition) {
  CVVertex* previous = 0;
  CVVertex* current = &root;

  while (current) {
    boost::unwrap_ref(visitor)(*current);
    if (boost::unwrap_ref(stop_condition)(*current))
      return current;

    previous = current;

    // NB: The two const_casts here to allow just one traversal policy taking
    // a vertex const*, instead of forcing users to define two policies,
    // one for vertex*, another for vertex const*.

    current = const_cast<CVVertex*>(
      boost::unwrap_ref(traversal_policy)(
        *const_cast<vertex const*>(current)
    ));
  }

  return previous;
}


template <typename CVVertex, typename TraversalPolicy, typename Visitor>
CVVertex* traverse(CVVertex& root, TraversalPolicy traversal_policy,
                   Visitor visitor) {
  return traverse(root, traversal_policy, visitor, null_stop_condition());
}


template <typename CVVertex, typename TraversalPolicy>
CVVertex* traverse(CVVertex& root, TraversalPolicy traversal_policy) {
  return traverse(root, traversal_policy, null_visitor(),
                  null_stop_condition());
}

//! Generic tree traverser like traverse, except that this one applies 
//! TraversalPolicy first and Visitor and StopCondition after that. Useful for 
//! post-order traversals.
template <typename TraversalPolicy, typename Visitor, typename StopCondition>
vertex* traverse_postorder(vertex& root, TraversalPolicy traversal_policy,
                           Visitor visitor, StopCondition stop_condition) {
  vertex* previous = &root;
  vertex* current = &*boost::unwrap_ref(traversal_policy)(root);

  while (current) {
    boost::unwrap_ref(visitor)(*current);
    if (boost::unwrap_ref(stop_condition)(*current))
      return current;

    previous = current;
    current = boost::unwrap_ref(traversal_policy)(*current);
  }

  return previous;
}

template <typename TraversalPolicy, typename Visitor>
vertex* traverse_postorder(vertex& root, TraversalPolicy traversal_policy,
                           Visitor visitor) {
  return traverse_postorder(root, traversal_policy, visitor,
                            null_stop_condition());
}

template <typename TraversalPolicy>
vertex* traverse_postorder(vertex& root, TraversalPolicy traversal_policy) {
  return traverse_postorder(root, traversal_policy, null_visitor(),
                            null_stop_condition());
}

class backtrack {
  // A stack of (current vertex on a level, end iterator for that level).
  typedef std::stack<
    std::pair<
      vertex::const_children_iterator,
      vertex::const_children_iterator
    >
  > stack_t;

public:
  vertex const* operator () (vertex const& current) {
    if (current.children_count() > 0) {
      // This vertex has any children? Good, go to the first one. Also push 
      // this level onto the stack.
      stack_.push(
        std::make_pair(current.children_begin(), current.children_end())
      );
      return &*current.children_begin();

    } else while (!stack_.empty()) {
        // A leaf? Okay, does it have an unvisited sibling?
        vertex::const_children_iterator& cur = stack_.top().first;
        vertex::const_children_iterator& end = stack_.top().second;

        ++cur;
        if (cur != end)
          return &*cur;   // It does, visit that.
        else
          stack_.pop();  // Nope, try one level above.
      }

    // No dice either way? Looks like we're done.
    return 0;
  }

private:
  stack_t stack_;
};

//! Postorder traveral policy for traverse_postorder. Do note that this never 
//! traverses the root vertex.
class postorder {
  typedef std::stack<
    std::pair<
      apns::vertex::const_children_iterator,
      apns::vertex::const_children_iterator
    >
  > stack_t;

public:
  vertex const* operator () (apns::vertex const& v) {
    using namespace apns;

    if (stack_.empty()) {
      if (v.children_count() > 0) {
        stack_.push(std::make_pair(v.children_begin(), v.children_end()));
        return inc(recurse(stack_.top().first));
      } else {
        return 0;
      }
    }

    if (stack_.top().first != stack_.top().second)
      return inc(recurse(stack_.top().first));
    else {
      while (!stack_.empty() && stack_.top().first == stack_.top().second)
        stack_.pop();

      if (!stack_.empty())
        return inc(stack_.top().first);
      else
        return 0;
    }
  }

private:
  stack_t stack_;

  //! If an iterator is given, increments it and returns pointer to the 
  //! originally pointed-to vertex. Otherwise, it returns 0.
  vertex const* inc(boost::optional<vertex::const_children_iterator&> i) {
    if (i)
      return &*((*i)++);
    else
      return 0;
  }

  boost::optional<vertex::const_children_iterator&>
  recurse(vertex::const_children_iterator from) {
    vertex::const_children_iterator current = from;
    while (current->children_count() > 0) {
      stack_.push(
        std::make_pair(current->children_begin(), current->children_end())
      );
      current = current->children_begin();
    }

    if (!stack_.empty())
      return stack_.top().first;
    else
      return boost::none;
  }
};

//! A visitor counting the vertices of the tree.
struct vertex_counter {
  vertex_counter() : count(0) { }

  void operator () (vertex const&) {
    ++count;
  }

  std::size_t count;
};

//! Holds an instance of a concrete traversal policy object and delegates the 
//! traversal to it.
struct virtual_traversal_policy {
  //! Virtual traversal policy interface.
  struct base {
    virtual ~base() { }
    virtual vertex const* next(vertex const&) = 0;
  };

  boost::shared_ptr<base> policy;  //!< Concrete traversal policy.

  explicit virtual_traversal_policy(boost::shared_ptr<base> const& policy) :
    policy(policy)
  { }

  vertex const* operator () (vertex const& current) {
    assert(policy);
    return policy->next(current);
  }
};

//! A visitor that holds two other visitors.
template <typename First, typename Second>
struct composite_visitor {
  First   first;
  Second  second;

  composite_visitor(First first = First(), Second second = Second()) :
    first(first),
    second(second)
  { }

  composite_visitor(Second second) :
    second(second)
  { }

  void operator () (vertex& v) {
    boost::unwrap_ref(first)(v);
    boost::unwrap_ref(second)(v);
  }
};

//! Helper to make a composite_visitor without having to type out the full 
//! template parameter list.
template <typename First, typename Second>
composite_visitor<First, Second> make_composite_visitor(First f, Second s) {
  return composite_visitor<First, Second>(f, s);
}

//! Holds an instance of a concrete visitor (derived from 
//! virtual_visitor::base) and delegates all visiting to it. Useful e.g. for 
//! use from Python.
struct virtual_visitor {
  //! Virtual visitor interface.
  struct base {
    virtual ~base() { }
    virtual void visit(vertex&) = 0;
  };

  boost::shared_ptr<base> visitor;  //!< Concrete virtual visitor.

  explicit virtual_visitor(boost::shared_ptr<base> const& visitor) :
    visitor(visitor) 
  { }

  void operator () (vertex& v) {
    assert(visitor.get() != 0);
    visitor->visit(v);
  }
};

//! Holds an instance of a concrete StopCondition (derived from 
//! virtual_stop_condition::base) and delegates the stop decision to it.
struct virtual_stop_condition {
  //! Virtual stop condition interface.
  struct base {
    virtual ~base() { }
    virtual bool stop(vertex&) = 0;
  };

  boost::shared_ptr<base> condition;  //!< Concrete stop condition.

  explicit virtual_stop_condition(boost::shared_ptr<base> const& condition) :
    condition(condition)
  { }

  bool operator () (vertex& v) {
    assert(condition);
    return condition->stop(v);
  }
};

} // namespace apns

#endif
