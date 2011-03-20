# build_config.py -- user-specific build configuration options.
# This file serves as a Python module that will be loaded from the SConstruct file during build.

from sys import platform

#
# Include paths for used libraries.
#

if platform == 'win32':
  BOOST_HEADERS = 'C:/Users/Oxyd/Development/boost_1_45_0/'
  BOOST_LIBS = 'C:/Users/Oxyd/Development/boost_1_45_0/stage/lib'

  PYTHON_HEADERS = 'C:/Python27/include'
  PYTHON_LIBS = 'C:/Python27/libs'
  
  GTEST_HEADERS = 'C:/Users/Oxyd/Development/gtest-1.5.0/include'
  GTEST_LIBS = 'C:/Users/Oxyd/Development/gtest-1.5.0/lib'

else:
  BOOST_HEADERS = '~/boost/boost_1_45_0/'
  BOOST_LIBS = '~/boost/boost_1_45_0/stage/lib'

  PYTHON_HEADERS = '/usr/local/include/python2.7/'
  PYTHON_LIBS = '/usr/local/lib/python2.7/'

  GTEST_HEADERS = '/usr/local/include/'
  GTEST_LIBS = '/usr/local/lib/'


#
# Library versions
#

# Set this to your version of Python. Good values are (2, 6) or (2, 7).
PYTHON_VERSION = (2, 7)

# Set this to your version of Boost libraries. Good values are e.g. '1_45' or '1_44'.
BOOST_VERSION = '1_45'

# Helper function: Given an x, return the tuple (x, x).
duplicate = lambda x: (x, x)

if platform == 'win32':
  # Set this to your version of Visual Studio that you used to compile Boost Libraries and will use
  # to compile APNS module. (Yes, they must be the same version.) Good values are 'vc100', 'vc9' or 'vc8'.
  VISUAL_STUDIO_VERSION = 'vc100'

  # These values are tuples. The first element is the ordinary library, the second element is the
  # debug version of the same library.

  pythonVerString = '%d%d' % (PYTHON_VERSION[0], PYTHON_VERSION[1])
  PYTHON_LIB = duplicate('python%s' % pythonVerString)

  # Use the auto-linking feature of the Microsoft compiler together with the fact that Boost Libraries use it too.
  BOOST_RANDOM_LIB = ('', '')
  BOOST_PYTHON_LIB = ('', '')
  
  GTEST_LIB = ('gtest', 'gtestd')

else:
  pythonVerString = '%d.%d' % (PYTHON_VERSION[0], PYTHON_VERSION[1])
  PYTHON_LIB = duplicate('python%s' % pythonVerString)

  BOOST_RANDOM_LIB = duplicate('boost_random')
  BOOST_PYTHON_LIB = duplicate('boost_python')

  GTEST_LIB = duplicate('gtest')

# Compiler version
if platform == 'win32':
  COMPILER = None  # Keep default.
  RPATH = None     # Makes no sense for MSVS.

else:
  COMPILER = '/usr/local/bin/g++45'  # Use G++ 4.5 which is not in base by default.
  RPATH = '/usr/local/lib/gcc45/'
