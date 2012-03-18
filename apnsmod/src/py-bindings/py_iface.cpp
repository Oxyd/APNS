/**
 * \file py_iface.cpp
 *
 * \brief C++/Python interface.
 *
 * The glue between C++ and Python code. This file defines the entry initialisation function which
 * exports public C++ types and functions for use from Python. There are certain differences between C++ and Python
 * identifiers. Namely, the C++ code uses the all_lowercase convention, while Python uses CamelCase for types and
 * lowerCamelCase for functions and variables.
 */

#include <boost/python.hpp>

void export_board();
void export_movement();
void export_tree();
void export_hash();
void export_util();
void export_search_algos();

//! Python module initialization entry point.
BOOST_PYTHON_MODULE(_apnsmod) {
  export_board();
  export_movement();
  export_tree();
  export_hash();
  export_util();
  export_search_algos();
}