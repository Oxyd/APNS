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
#include <boost/container/vector.hpp>
#include <boost/move/move.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_convertible.hpp>

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
class vertex {
  typedef boost::container::vector<vertex> children_container;

  //! This is to provide the op ->* that is for some reason missing in 
  //! boost::vector's iterator...
  template <typename Iter>
  class iterator_base : public boost::iterator_adaptor<
    iterator_base<Iter>,
    Iter
  > {
    struct enabler { };

  public:
    iterator_base() { }

    template <typename Other>
    iterator_base(iterator_base<Other> const& other,
                  typename boost::enable_if<boost::is_convertible<Other, Iter>, enabler>::type = enabler())
      : iterator_base::iterator_adaptor_(other.base()) { }

    /*implicit*/ iterator_base(Iter i)
      : iterator_base::iterator_adaptor_(i) { }

    template <typename T>
    T& operator ->* (T vertex::*mptr) { return (*(this->base())).*mptr; }
  };

public:
  //! Type of the proof and disproof numbers.
  typedef boost::uint32_t number_t;  
  
  typedef iterator_base<children_container::iterator>       iterator;
  typedef iterator_base<children_container::const_iterator> const_iterator;
  typedef boost::reverse_iterator<iterator>                 reverse_iterator;
  typedef boost::reverse_iterator<const_iterator>           const_reverse_iterator;

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
  std::size_t subtree_size;
  signed char steps_remaining : 4;
  e_type      type            : 1;

  vertex()
    : proof_number(0)
    , disproof_number(0)
    , subtree_size(0)
    , steps_remaining(0)
    , type(vertex::type_or) { }

  vertex(BOOST_RV_REF(vertex) other)
    : proof_number(other.proof_number)
    , disproof_number(other.disproof_number)
    , subtree_size(other.subtree_size)
    , steps_remaining(other.steps_remaining)
    , type(other.type)
    , children_(boost::move(other.children_)) { }

  vertex& operator = (BOOST_RV_REF(vertex) other) {
    proof_number = other.proof_number;
    disproof_number = other.disproof_number;
    steps_remaining = other.steps_remaining;
    type = other.type;
    subtree_size = other.subtree_size;
    children_ = boost::move(other.children_);
    return *this;
  }

  //! Add a child at the end of the children list and return an iterator to it.
  //! May invalidate all existing iterators to children of this vertex.
  iterator add() { return children_.insert(children_.end(), vertex()); }

  //! Remove child specified by an iterator into this vertex. May invalidate 
  //! all existing iterators to children of this vertex.
  //! \returns Iterator to the child following the removed one.
  iterator remove(const_iterator child) { return children_.erase(child.base()); }
  
  iterator               begin()        { return children_.begin(); }
  iterator               end()          { return children_.end(); }
  const_iterator         begin() const  { return children_.begin(); }
  const_iterator         end() const    { return children_.end(); }
  reverse_iterator       rbegin()       { return reverse_iterator(end()); }
  reverse_iterator       rend()         { return reverse_iterator(begin()); }
  const_reverse_iterator rbegin() const { return const_reverse_iterator(end()); }
  const_reverse_iterator rend() const   { return const_reverse_iterator(begin()); }

  std::size_t size() const          { return children_.size(); }
  bool        leaf() const          { return children_.empty(); }

  /// Size of the tree rooted in this vertex in bytes.
  std::size_t subtree_bytes() const { return subtree_size * sizeof(vertex); }

  //! Construct an iterator to a child from a pointer to a child of this 
  //! vertex. The behaviour is undefined if child does not point to a child of 
  //! this vertex.
  //!
  //! \note This is surprisingly ineffective (O(n) instead of O(1)) -- could
  //!   it be improved or removed?
  iterator iter_from_ptr(vertex* child) {
    namespace bl = boost::lambda;
    return std::find_if(children_.begin(), children_.end(), &bl::_1 == child);
  }

  //! Sort the children of this vertex according to a comparator. This sort is stable.
  template <typename Compare>
  void sort(Compare comp) {
    stable_sort(comp);
  }

  //! Stable sort the children of this vertex according to a comparator.
  template <typename Compare>
  void stable_sort(Compare comp) {
    boost::container::vector<std::size_t> indicies;
    indicies.reserve(children_.size());
    for (std::size_t i = 0; i < children_.size(); ++i) indicies.push_back(i);

    std::stable_sort(indicies.begin(), indicies.end(), indirect_compare<Compare>(children_, comp));

    children_container temp;
    temp.reserve(children_.size());
    for (boost::container::vector<std::size_t>::const_iterator i = indicies.begin(); i != indicies.end(); ++i)
      temp.push_back(boost::move(children_[*i]));

    children_ = boost::move(temp);
  }

  //! Swap two children of this vertex.
  void swap_children(iterator first, iterator second);

  //! Make this vertex allocate enough memory to hold new_size children, but 
  //! don't really create the children yet. This may invalidate all existing 
  //! iterators to children of this vertex.
  void reserve(std::size_t new_size) { children_.reserve(new_size); }

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
  void resize(std::size_t new_size) { children_.resize(new_size); }

  //! Make this vertex use only as much memory as required to hold all its 
  //! children. This may invalidate all existing iterator
  //! to children of this vertex.
  void pack() { children_.shrink_to_fit(); }
  
private:
  BOOST_MOVABLE_BUT_NOT_COPYABLE(vertex)

  children_container children_;

  template <typename Cmp>
  class indirect_compare {
  public:
    indirect_compare(children_container const& children, Cmp cmp)
      : children_(&children)
      , cmp_(cmp) { }

    bool operator () (std::size_t x, std::size_t y) const {
      return cmp_((*children_)[x], (*children_)[y]);
    }

  private:
    children_container const* children_;
    Cmp                       cmp_;
  };
};

} // namespace apns

namespace std { template <> inline void swap<>(apns::vertex& x, apns::vertex& y) {
  apns::vertex tmp(boost::move(x));
  x = boost::move(y);
  y = boost::move(tmp);
} }

namespace apns {

inline void vertex::swap_children(vertex::iterator x, vertex::iterator y) { std::swap(*x, *y); }

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
  parent.sort(comp);
}

template <typename Compare>
void stable_sort_children(vertex& parent, Compare comp = Compare()) {
  parent.stable_sort(comp);
}

/// Calculate proper values for the .subtree_size members in the whole tree.
void calculate_sizes(vertex& root);

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
    root.subtree_size     = 1;
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
template <typename CVVertex, typename TraversalPolicy, typename Visitor, typename StopCondition>
CVVertex* traverse_postorder(CVVertex& root, TraversalPolicy traversal_policy,
                             Visitor visitor, StopCondition stop_condition) {
  CVVertex* previous = &root;
  CVVertex* current = const_cast<CVVertex*>(
    &*boost::unwrap_ref(traversal_policy)(
      const_cast<vertex const&>(root)
  ));

  while (current) {
    boost::unwrap_ref(visitor)(*current);
    if (boost::unwrap_ref(stop_condition)(*current))
      return current;

    previous = current;
    current = const_cast<CVVertex*>(
      boost::unwrap_ref(traversal_policy)(
        *const_cast<vertex const*>(current)
    ));
  }

  return previous;
}

template <typename CVVertex, typename TraversalPolicy, typename Visitor>
CVVertex* traverse_postorder(CVVertex& root, TraversalPolicy traversal_policy,
                             Visitor visitor) {
  return traverse_postorder(root, traversal_policy, visitor, null_stop_condition());
}

template <typename CVVertex, typename TraversalPolicy>
CVVertex* traverse_postorder(CVVertex& root, TraversalPolicy traversal_policy) {
  return traverse_postorder(root, traversal_policy, null_visitor(), null_stop_condition());
}

class backtrack {
  // A stack of (current vertex on a level, end iterator for that level).
  typedef std::stack<
    std::pair<
      vertex::const_iterator,
      vertex::const_iterator
    >
  > stack_t;

public:
  vertex const* operator () (vertex const& current) {
    if (current.size() > 0) {
      // This vertex has any children? Good, go to the first one. Also push 
      // this level onto the stack.
      stack_.push(
        std::make_pair(current.begin(), current.end())
      );
      return &*current.begin();

    } else while (!stack_.empty()) {
        // A leaf? Okay, does it have an unvisited sibling?
        vertex::const_iterator& cur = stack_.top().first;
        vertex::const_iterator& end = stack_.top().second;

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
      apns::vertex::const_iterator,
      apns::vertex::const_iterator
    >
  > stack_t;

public:
  vertex const* operator () (apns::vertex const& v) {
    using namespace apns;

    if (stack_.empty()) {
      if (v.size() > 0) {
        stack_.push(std::make_pair(v.begin(), v.end()));
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
  vertex const* inc(boost::optional<vertex::const_iterator&> i) {
    if (i)
      return &*((*i)++);
    else
      return 0;
  }

  boost::optional<vertex::const_iterator&>
  recurse(vertex::const_iterator from) {
    vertex::const_iterator current = from;
    while (current->size() > 0) {
      stack_.push(
        std::make_pair(current->begin(), current->end())
      );
      current = current->begin();
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

//! Get the size of the subtree rooted in a given vertex, including the root
//! itself. (E.g. subtree_size(a_leaf) == 1.)
inline std::size_t subtree_size(vertex const& v) {
  vertex_counter counter;
  traverse(v, backtrack(), boost::ref(counter));
  return counter.count;
}

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
