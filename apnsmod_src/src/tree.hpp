#ifndef TREE_HPP
#define TREE_HPP

#include "hash.hpp"
#include "board.hpp"

#include <boost/optional.hpp>
#include <boost/utility.hpp>
#include <boost/scoped_array.hpp>
#include <boost/cstdint.hpp>

#include <cstddef>
#include <cassert>

//! A vertex of the game tree.
//!
//! Vertices are non-copyable to prevent costly copies of large subtrees. Internally, they are implemented via resizeable
//! array -- as such, their allocated size may be larger than the number of children. The pack() member function can be used
//! to make the vertex only take up as much free-store space as required.
//!
//! \note This class does some intricate memory management stuff to improve performance of the search algorithm. This requires
//! that this class be a POD type.
//!
//! \note This class behaves like a container of children. The children are always sorted in the order in which they were
//! inserted.
//!
//! \note Operations such as add_child, remove_child, reserve, resize and pack may and will invalidate all iterators, pointers
//! and references to any children of this vertex.
class vertex {
public:
  typedef boost::uint32_t number_t;  //!< Type of the proof and disproof numbers.

  typedef vertex*         children_iterator;
  typedef vertex const*   const_children_iterator;
  
  static number_t const max_num;  //!< Maximum value of a proof- or disproof number.
  static number_t const infty;    //!< Infinity value used in the algorithm.
  
  //! Type of this node.
  enum e_type {
    type_and,   //!< Defender's turn.
    type_or     //!< Attacker's turn.
  };

  number_t                  proof_number;
  number_t                  disproof_number;
  boost::optional< ::step>  step;
  int                       steps_remaining;
  e_type                    type;

  vertex();
  ~vertex();

  //! Add a child at the end of the children list and return an iterator to it. May invalidate all existing iterators to
  //! children of this vertex.
  children_iterator add_child();

  //! Remove child specified by an iterator into this vertex. May invalidate all existing iterators to children of this vertex.
  void remove_child(children_iterator child);
  
  children_iterator       children_begin()          { return reinterpret_cast<vertex*>(storage.get()); }
  children_iterator       children_end()            { return children_begin() + size; }
  const_children_iterator children_begin() const    { return reinterpret_cast<vertex const*>(storage.get()); }
  const_children_iterator children_end() const      { return children_begin() + size; }

  std::size_t             children_count() const    { return children_end() - children_begin(); }

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
  
private:
  //! Memory allocation interface. It is here so that it can be changed easily, but also so that a change of the allocator
  //! doesn't change the type of vertex as would happen with a template parameter. This is justified by my assumption that only
  //! one type of vertices (all using the same allocator) will be needed in this program.
  struct allocator {
    //! Allocate memory to hold count objects of type vertex.
    //!
    //! \returns Pointer to new memory or null if memory has been exhausted.
    static char* allocate(std::size_t count) throw ();

    //! Reclaim previously-allocated memory.
    //!
    //! \param memory Pointer to previously-allocated memory. May be null.
    static void deallocate(char* memory) throw ();
  };

  //! A SBRM wrapper for a non-copyable, non-transferrable arrays of char. This mimics boost::scoped_array, except it uses
  //! allocator::deallocate to deallocate the storage, unlikely boost::scoped_array which always uses delete-expression.
  class storage_wrapper {
    // Safe bool idiom.
    typedef void (storage_wrapper::* bool_type)() const;
    void this_type_does_not_support_comparisons() const { }

  public:
    typedef char element_type;

    explicit storage_wrapper(element_type* ptr = 0) : storage(ptr) { }
    ~storage_wrapper() throw ()                                 { reset(); }

    void reset(element_type* new_ptr = 0) throw ();

    element_type& operator [] (std::ptrdiff_t i) const throw () { return *(get() + i); }
    element_type* get() const throw ()                          { return storage; }

    operator bool_type() const {
      return storage ? &storage_wrapper::this_type_does_not_support_comparisons : 0;
    }

    void swap(storage_wrapper& other) throw ();

  private:
    element_type* storage;
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
};

//! Given a vertex type, return its opposite.
inline vertex::e_type opposite_type(vertex::e_type t) {
  return t == vertex::type_or ? vertex::type_and : vertex::type_or;
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

//! A no-op visitor for traverser.
struct null_visitor {
  void operator () (vertex const&) { }
};

//! Convenience typedef so that users don't have to type out the function signature each time they want to use a traverser
//! with an ordinary function.
typedef vertex* (*fun_tp)(vertex&);

//! Generic tree traverser. This traverser doesn't have to visit every node in the tree; instead, the path it will take is
//! dictated by the specified TraversalPolicy.
//!
//! \tparam TraversalPolicy Specifies how the tree will be traversed.
//! \tparam Visitor Will be called on each node that is traversed.
template <typename TraversalPolicy, typename Visitor = null_visitor>
struct traverser {
  TraversalPolicy traversal_policy;
  Visitor         visitor;

  explicit traverser(TraversalPolicy traversal_policy = TraversalPolicy(), Visitor visitor = Visitor()) :
    traversal_policy(traversal_policy),
    visitor(visitor)
  { }

  explicit traverser(Visitor visitor) :
    visitor(visitor)
  { }

  //! Traverse the tree rooted at root.
  //!
  //! \returns The last visited vertex.
  vertex::children_iterator traverse(vertex& root) {
    // This internally assumes that vertex::children_iterator is vertex*. If that changes, this function will have to be
    // rewritten.
    vertex* previous = &root;
    vertex* current = &root;

    while (current) {
      visitor(*current);
      previous = current;
      current = traversal_policy(*current);
    }

    return previous;
  }
};

//! Holds an instance of a concrete traversal policy object and delegates the traversal to it.
struct virtual_traversal_policy {
  //! Virtual traversal policy interface.
  struct base {
    virtual ~base() { }
    virtual vertex::children_iterator next(vertex&) = 0;
  };

  boost::shared_ptr<base> policy;  //!< Concrete traversal policy.

  explicit virtual_traversal_policy(boost::shared_ptr<base> const& policy) :
    policy(policy)
  { }

  vertex::children_iterator operator () (vertex& current) {
    assert(policy);
    return policy->next(current);
  }
};

//! Helper to compose two visitors. This allows to use an arbitrary number of visitors with a single traverser.
//! Visitors are applied in specified order.
template <typename First, typename Second>
struct composite_visitor {
  First   first;
  Second  second;

  explicit composite_visitor(First f = First(), Second s = Second()) :
    first(f),
    second(s)
  { }

  void operator () (vertex& v) {
    first(v);
    second(v);
  }
};

//! Helper function to create a composite visitor.
template <typename First, typename Second>
composite_visitor<First, Second>
make_composite_visitor(First f, Second s) {
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

#endif
