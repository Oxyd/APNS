#ifndef PY_BINDINGS_PY_UTILS
#define PY_BINDINGS_PY_UTILS

#include <boost/python.hpp>

//! Convert a C++ sequence into a Python list.
template <typename Iterator>
boost::python::list py_list_from_cpp_seq(Iterator begin, Iterator end) {
  using namespace boost::python;

  list result;
  while (begin != end) {
    result.append(*begin++);
  }

  return result;
}

//! Make a C++ sequence from a Python list.
template <typename Container>
void cpp_seq_from_py_list(boost::python::list const& list, Container& cont) {
  using namespace boost::python;

  for (int index = 0; index < len(list); ++index) {
    cont.insert(cont.end(),
        extract<typename Container::value_type>(list[index]));
  }
}

/**
 * \brief Converter from \c optional<T> to \c T or \c None.
 *
 * This is for use with \c to_python_converter. Any object \c o of type \c boost::optional<T> will be converted
 * either to \c None if \c o is empty, or to \c *o otherwise.
 */
template <typename T>
struct optional_to_T {
  static PyObject* convert(boost::optional<T> o) {
    using namespace boost::python;

    if (o) {
      return incref(object(*o).ptr());
    } else {
      return Py_None;
    }
  }
};

/**
 * \brief Converter from \c std::pair<A, B> to Python tuple.
 *
 * This is for use with \c to_python_converter. If \c p is a \c std::pair<A, B>, then \c p will be converted
 * to <tt>(p.first, p.second)</tt>.
 */
template <typename A, typename B>
struct pair_to_tuple {
  static PyObject* convert(std::pair<A, B> p) {
    using namespace boost::python;

    return incref(boost::python::make_tuple(p.first, p.second).ptr());
  }
};

#endif

