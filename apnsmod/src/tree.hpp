#ifndef TREE_HPP
#define TREE_HPP

#include "board.hpp"
#include "movement.hpp"
#include "util.hpp"

#include <boost/optional.hpp>
#include <boost/utility.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/cstdint.hpp>
#include <boost/ref.hpp>
#include <boost/iterator/iterator_adaptor.hpp>
#include <boost/iterator/reverse_iterator.hpp>
#include <boost/type_traits.hpp>

#include <cstddef>
#include <cassert>
#include <iterator>
#include <stack>

namespace apns {

//! A vertex of the game tree.
//!
//! Vertices are non-copyable to prevent costly copies of large subtrees. Internally, they are implemented via resizeable
//! array -- as such, their allocated size may be larger than the number of children. The pack() member function can be used
//! to make the vertex only take up as much free-store space as required.
//!
//! \note This class behaves like a container of children. The children are always sorted in the order in which they were
//! inserted.
//!
//! \note Operations such as add_child, remove_child, reserve, resize and pack may and will invalidate all iterators, pointers
//! and references to any children of this vertex.
class vertex {
  template <typename HeldValue>
  class children_iterator_base : public boost::iterator_adaptor<
    children_iterator_base<HeldValue>,
    HeldValue*,
    boost::use_default,
    boost::bidirectional_traversal_tag
  > {
    struct enabler { };

  public:
    children_iterator_base() { }
    explicit children_iterator_base(HeldValue* ptr) : children_iterator_base::iterator_adaptor_(ptr) { }

    template <typename OtherValue>
      children_iterator_base(
        children_iterator_base<OtherValue> const& other,
        typename boost::enable_if<boost::is_convertible<OtherValue*, HeldValue*>, enabler>::type = enabler()
      ) : children_iterator_base::iterator_adaptor_(other.base()) { }

    template <typename T>
    T& operator ->* (T HeldValue::* mptr) { return this->base()->*mptr; }
  };

public:
  typedef boost::uint32_t number_t;  //!< Type of the proof and disproof numbers.
  
  typedef children_iterator_base<vertex>                    children_iterator;
  typedef children_iterator_base<vertex const>              const_children_iterator;
  typedef boost::reverse_iterator<children_iterator>        reverse_children_iterator;
  typedef boost::reverse_iterator<const_children_iterator>  const_reverse_children_iterator;

  static number_t const max_num;  //!< Maximum value of a proof- or disproof number.
  static number_t const infty;    //!< Infinity value used in the algorithm.
  
  //! Type of this node.
  enum e_type {
    type_and,   //!< Defender's turn.
    type_or     //!< Attacker's turn.
  };

  number_t                  proof_number;
  number_t                  disproof_number;
  step_holder               step;
  int                       steps_remaining;
  e_type                    type;

  vertex();
  ~vertex();

  //! Add a child at the end of the children list and return an iterator to it. May invalidate all existing iterators to
  //! children of this vertex.
  children_iterator add_child();

  //! Remove child specified by an iterator into this vertex. May invalidate all existing iterators to children of this vertex.
  void remove_child(children_iterator child);
  
  children_iterator children_begin() {
    return children_iterator(reinterpret_cast<vertex*>(storage.get()));
  }

  children_iterator children_end() {
    return children_iterator(reinterpret_cast<vertex*>(storage.get() + size));
  }

  const_children_iterator children_begin() const {
    return const_children_iterator(reinterpret_cast<vertex const*>(storage.get()));
  }

  const_children_iterator children_end() const {
    return const_children_iterator(reinterpret_cast<vertex const*>(storage.get() + size));
  }

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

  std::size_t children_count() const { return size; }

  //! Construct an iterator to a child from a pointer to a child of this vertex. The behaviour is undefined if child does not
  //! point to a child of this vertex.
  children_iterator iter_from_ptr(vertex* child) {
    return children_iterator(child);
  }

  //! Make this vertex allocate enough memory to hold new_size children, but don't really create the children yet. This may
  //! invalidate all existing iterators to children of this vertex.
  void reserve(std::size_t new_size);

  //! Make this vertex have new_size children. If new_size is more than the current size, new children will be 
  //! default-constructed at the end of the children sequence. If new_size is less than the current size, this is equivalent
  //! to removing children from the end until this vertex has exactly new_size children.
  //!
  //! This function may invalidate all existing iterators to children of this vertex.
  //!
  //! \note This doesn't necessarily free up any memory if the size is reduced.
  void resize(std::size_t new_size);

  //! Make this vertex use only as much memory as required to hold all its children. This may invalidate all existing iterator
  //! to children of this vertex.
  void pack();

  //! Swap the subtree rooted in this vertex with a subtree rooted in the other vertex.
  void swap(vertex& other);

  //! Get the total memory usage by all vertices of any tree.
  static std::size_t alloc_size() { return allocator::alloc; }
  
private:
  //! Memory allocation interface. It is here so that it can be changed easily, but also so that a change of the allocator
  //! doesn't change the type of vertex as would happen with a template parameter. This is justified by my assumption that only
  //! one type of vertices (all using the same allocator) will be needed in this program.
  struct allocator {
    //! Allocate memory to hold count objects of type vertex.
    //!
    //! \returns Pointer to new memory.
    //! \throws std::bad_alloc
    static vertex* allocate(std::size_t count);

    //! Reclaim previously-allocated memory.
    //!
    //! \param memory Pointer to previously-allocated memory. May be null.
    static void deallocate(vertex* memory, std::size_t count) throw ();

    static std::size_t alloc;
  };

  //! A SBRM wrapper for a non-copyable, non-transferrable arrays of vertex. This mimics boost::scoped_array, except it uses
  //! allocator::deallocate to deallocate the storage, unlikely boost::scoped_array which always uses delete-expression.
  class storage_wrapper {
    // Safe bool idiom.
    typedef void (storage_wrapper::* bool_type)() const;
    void this_type_does_not_support_comparisons() const { }

  public:
    typedef vertex element_type;

    storage_wrapper(element_type* ptr = 0, std::size_t size = 0) : storage(ptr), size(size) { }
    ~storage_wrapper() throw ()                                 { reset(0, 0); }

    void reset(element_type* new_ptr, std::size_t size) throw ();

    element_type& operator [] (std::ptrdiff_t i) const throw () { return *(get() + i); }
    element_type* get() const throw ()                          { return storage; }

    operator bool_type() const {
      return storage ? &storage_wrapper::this_type_does_not_support_comparisons : 0;
    }

    void swap(storage_wrapper& other) throw ();

  private:
    element_type* storage;
    std::size_t   size;
  };

  storage_wrapper storage;
  std::size_t     size;
  std::size_t     capacity;

  // These two in fact move rather than copy.
  vertex(vertex& other);
  vertex& operator = (vertex& other);

  //! Make the storage large enough to hold exactly new_alloc children. This assumes that size <= new_alloc as it has to copy
  //! old children into new storage. If new_alloc is 0, doesn't allocate any storage but merely deallocates the previous one.
  void realloc(std::size_t new_alloc);

  //! Get a child with a given index. Doesn't do any bounds checking.
  vertex& get(std::size_t index);

  //! Destroy this vertex assuming that its children have been destroyed too (regardless of what capacity and size have to say).
  void destroy();
};

//! Given a vertex type, return its opposite.
inline vertex::e_type opposite_type(vertex::e_type t) {
  return t == vertex::type_or ? vertex::type_and : vertex::type_or;
}

namespace detail {

template <typename Compare>
struct ptr_compare {
  explicit ptr_compare(Compare comp = Compare()) : comp(comp) { }
  bool operator () (vertex const* lhs, vertex const* rhs) {
    return comp(*lhs, *rhs);
  }

private:
  Compare comp;
};

} // namespace detail

//! Sort the children of a vertex according to a comparator.
template <typename Compare>
void sort_children(vertex& parent, Compare comp = Compare()) {
  std::vector<vertex*> temp;
  temp.reserve(parent.children_count());
  for (vertex::children_iterator c = parent.children_begin(); c != parent.children_end(); ++c)
    temp.push_back(&*c);

  std::sort(temp.begin(), temp.end(), detail::ptr_compare<Compare>(comp));

  vertex new_parent;
  new_parent.proof_number     = parent.proof_number;
  new_parent.disproof_number  = parent.disproof_number;
  new_parent.step             = parent.step;
  new_parent.steps_remaining  = parent.steps_remaining;
  new_parent.type             = parent.type;
  new_parent.resize(parent.children_count());

  vertex::children_iterator dest = new_parent.children_begin();
  for (std::vector<vertex*>::const_iterator src = temp.begin(); src != temp.end(); ++src, ++dest)
    std::swap(*dest, **src);

  std::swap(parent, new_parent);
}

//! Update the order of children of a vertex assuming it was sorted previously but child is now out of order.
//!
//! \returns Iterator to the given child in its new position.
template <typename Compare>
vertex::children_iterator resort_children(vertex& parent, vertex::children_iterator child, Compare comp = Compare()) {
  // So long as the child is greater than its sibling to the right, bubble it right.
  while (boost::next(child) != parent.children_end() && comp(*boost::next(child), *child)) {
    vertex::children_iterator equal_range_end = boost::next(child);
    while (boost::next(equal_range_end) != parent.children_end() && !comp(*equal_range_end, *boost::next(equal_range_end)))
      ++equal_range_end;

    std::swap(*child, *equal_range_end);
    child = equal_range_end;
  }

  // And conversly, so long as the sibling to the left is greater or equal than the child, bubble left. Note that this won't do
  // anything if the previous loop already re-sorted the children.
  while (child != parent.children_begin() && !comp(*boost::prior(child), *child)) {
    vertex::children_iterator equal_range_begin = boost::prior(child);
    while (boost::prior(equal_range_begin) != parent.children_begin() && !comp(*boost::prior(equal_range_begin), *equal_range_begin))
      --equal_range_begin;

    std::swap(*child, *boost::prior(child));
    --child;
  }

  return child;
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
//! \param filename Name of the file to save to. This will be opened by an ofstream.
//! \param op_ctrl OperationController for this operation. This algorithm does not provide information about its progress.
//!
//! \throws std::runtime_error Thrown if the file could not be written successfully.
void save_game(boost::shared_ptr<game> const& game, std::string const& filename, operation_controller& op_ctrl);
inline void save_game(boost::shared_ptr<game> const& game, std::string const& filename) {
  null_operation_controller op_ctrl;
  save_game(game, filename, op_ctrl);
}

//! Load a game from a file.
//!
//! \param filename File to be read. This will be opened by an ifstream.
//! \param op_ctrl OperationController for this operation. This algorithm does not provide information about its progress.
//!
//! \returns A new Game instance and total number of vertices in the tree.
//!
//! \throws std::runtime_error Thrown if the file could not be read successfully, or if its format is not valid.
std::pair<boost::shared_ptr<game>, std::size_t> load_game(std::string const& filename, operation_controller& op_ctrl);
inline std::pair<boost::shared_ptr<game>, std::size_t> load_game(std::string const& filename) {
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

//! Convenience typedef so that users don't have to type out the function signature each time they want to use a traverser
//! with an ordinary function.
typedef vertex* (*fun_tp)(vertex&);

//! Generic tree traverser. This traverser doesn't have to visit every node in the tree; instead, the path it will take is
//! dictated by the specified TraversalPolicy.
//!
//! \tparam TraversalPolicy Specifies how the tree will be traversed. It is a functor taking a vertex& and returning a pointer
//!   to the desired successor, or null if the traversal is to be stopped.
//! \tparam Visitor Will be called on each node that is traversed.
//! \tparam StopCondition Will be called on each node that is traversed (after Visitor) and if it returns true, the traversal
//! will be stopped and the last visited vertex returned.
template <typename TraversalPolicy, typename Visitor, typename StopCondition>
vertex* traverse(vertex& root, TraversalPolicy traversal_policy, Visitor visitor, StopCondition stop_condition) {
  vertex* previous = 0;
  vertex* current = &root;

  while (current) {
    boost::unwrap_ref(visitor)(*current);
    if (boost::unwrap_ref(stop_condition)(*current))
      return current;

    previous = current;
    current = &*boost::unwrap_ref(traversal_policy)(*current);
  }

  return previous;
}

template <typename TraversalPolicy, typename Visitor>
vertex* traverse(vertex& root, TraversalPolicy traversal_policy, Visitor visitor) {
  return traverse(root, traversal_policy, visitor, null_stop_condition());
}

template <typename TraversalPolicy>
vertex* traverse(vertex& root, TraversalPolicy traversal_policy) {
  return traverse(root, traversal_policy, null_visitor(), null_stop_condition());
}

//! Generic tree traverser like traverse, except that this one applies TraversalPolicy first and Visitor and StopCondition after
//! that. Useful for post-order traversals.
template <typename TraversalPolicy, typename Visitor, typename StopCondition>
vertex* traverse_postorder(vertex& root, TraversalPolicy traversal_policy, Visitor visitor, StopCondition stop_condition) {
  vertex* previous = &root;
  vertex* current = &*boost::unwrap_ref(traversal_policy)(root);

  while (current) {
    boost::unwrap_ref(visitor)(*current);
    if (boost::unwrap_ref(stop_condition)(*current))
      return current;

    previous = current;
    current = &*boost::unwrap_ref(traversal_policy)(*current);
  }

  return previous;
}

template <typename TraversalPolicy, typename Visitor>
vertex* traverse_postorder(vertex& root, TraversalPolicy traversal_policy, Visitor visitor) {
  return traverse_postorder(root, traversal_policy, visitor, null_stop_condition());
}

template <typename TraversalPolicy>
vertex* traverse_postorder(vertex& root, TraversalPolicy traversal_policy) {
  return traverse_postorder(root, traversal_policy, null_visitor(), null_stop_condition());
}

class backtrack {
  // A stack of (current vertex on a level, end iterator for that level).
  typedef std::stack<std::pair<vertex::children_iterator, vertex::children_iterator> > stack_t;

public:
  vertex* operator () (vertex& current) {
    if (current.children_count() > 0) {
      // This vertex has any children? Good, go to the first one. Also push this level onto the stack.
      stack.push(std::make_pair(current.children_begin(), current.children_end()));
      return &*current.children_begin();

    } else while (!stack.empty()) {
        // A leaf? Okay, does it have an unvisited sibling?
        vertex::children_iterator& cur = stack.top().first;
        vertex::children_iterator& end = stack.top().second;

        ++cur;
        if (cur != end)
          return &*cur;   // It does, visit that.
        else
          stack.pop();  // Nope, try one level above.
      }

    // No dice either way? Looks like we're done.
    return 0;
  }

private:
  stack_t stack;
};

//! Postorder traveral policy for traverse_postorder. Do note that this never traverses the root vertex.
class postorder {
  typedef std::stack<std::pair<apns::vertex::children_iterator, apns::vertex::children_iterator> > stack_t;

public:
  vertex* operator () (apns::vertex& v) {
    using namespace apns;

    if (stack.empty()) {
      if (v.children_count() > 0) {
        stack.push(std::make_pair(v.children_begin(), v.children_end()));
        return inc(recurse(stack.top().first));
      } else {
        return 0;
      }
    }

    if (stack.top().first != stack.top().second)
      return inc(recurse(stack.top().first));
    else {
      while (!stack.empty() && stack.top().first == stack.top().second)
        stack.pop();

      if (!stack.empty())
        return inc(stack.top().first);
      else
        return 0;
    }
  }

private:
  stack_t stack;

  //! If an iterator is given, increments it and returns pointer to the originally pointed-to vertex. Otherwise, it returns 0.
  vertex* inc(boost::optional<vertex::children_iterator&> i) {
    if (i)
      return &*((*i)++);
    else
      return 0;
  }

  boost::optional<vertex::children_iterator&> recurse(vertex::children_iterator from) {
    using namespace apns;

    vertex::children_iterator current = from;
    while (current->children_count() > 0) {
      stack.push(std::make_pair(current->children_begin(), current->children_end()));
      current = current->children_begin();
    }

    if (!stack.empty())
      return stack.top().first;
    else
      return boost::none;
  }
};

//! A visitor counting the vertices of the tree.
struct vertex_counter {
  vertex_counter() : count(0) { }

  void operator () (vertex&) {
    ++count;
  }

  std::size_t count;
};

//! Holds an instance of a concrete traversal policy object and delegates the traversal to it.
struct virtual_traversal_policy {
  //! Virtual traversal policy interface.
  struct base {
    virtual ~base() { }
    virtual vertex* next(vertex&) = 0;
  };

  boost::shared_ptr<base> policy;  //!< Concrete traversal policy.

  explicit virtual_traversal_policy(boost::shared_ptr<base> const& policy) :
    policy(policy)
  { }

  vertex* operator () (vertex& current) {
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

//! Helper to make a composite_visitor without having to type out the full template parameter list.
template <typename First, typename Second>
composite_visitor<First, Second> make_composite_visitor(First f, Second s) {
  return composite_visitor<First, Second>(f, s);
}

//! Holds an instance of a concrete visitor (derived from virtual_visitor::base) and delegates all visiting to it.
//! Useful e.g. for use from Python.
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

//! Holds an instance of a concrete StopCondition (derived from virtual_stop_condition::base) and delegates the stop decision
//! to it.
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

namespace std {
  template <> inline void swap(apns::vertex& x, apns::vertex& y) { x.swap(y); } 
}

#endif
