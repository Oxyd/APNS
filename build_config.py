# build_config.py -- user-specific build configuration options.
# This file serves as a Python module that will be loaded from the SConstruct file during build.

class BuildConfig(object):
  '''Merely a container for configuration variables set by buildConfig.'''
  pass

def buildConfig(platform, bits):
  config = BuildConfig()
  
  #
  # Include paths for used libraries.
  #
  
  if platform == 'win32' and bits == 64:
    config.BOOST_HEADERS = 'C:/Users/Oxyd/Development/boost_1_45_0/'
    config.BOOST_LIBS = 'C:/Users/Oxyd/Development/boost_1_45_0/stage/lib'
  
    config.PYTHON_HEADERS = 'C:/Python27/include'
    config.PYTHON_LIBS = 'C:/Python27/libs'
    
    config.GTEST_HEADERS = 'C:/Users/Oxyd/Development/gtest-1.5.0/include'
    config.GTEST_LIBS = 'C:/Users/Oxyd/Development/gtest-1.5.0/lib'
  
  elif platform == 'win32' and bits == 32:
    config.BOOST_HEADERS = 'C:/Users/Oxyd/Development/boost_1_45_0-32/'
    config.BOOST_LIBS = 'C:/users/Oxyd/Development/boost_1_45_0-32/stage/lib'
    
    config.PYTHON_HEADERS = 'C:/Python27-32/include'
    config.PYTHON_LIBS = 'C:/Python27-32/libs'
    
    config.GTEST_HEADERS = 'C:/Users/Oxyd/Development/gtest-1.5.0/inclue'
    config.GTEST_LIBS = 'C:/Users/Oxyd/Development/gtest-1.5.0/lib'
  
  else:  # FreeBSD, both 32-bit and 64-bit.
    config.BOOST_HEADERS = '/usr/local/include'
    config.BOOST_LIBS = '/usr/local/include'
  
    config.PYTHON_HEADERS = '/usr/local/include/python2.7/'
    config.PYTHON_LIBS = '/usr/local/lib/python2.7/'
  
    config.GTEST_HEADERS = '/usr/local/include/'
    config.GTEST_LIBS = '/usr/local/lib/'
  
  
  #
  # Library versions
  #
  
  # Set this to your version of Python. Good values are (2, 6) or (2, 7).
  config.PYTHON_VERSION = (2, 7)
  
  # Set this to your version of Boost libraries. Good values are e.g. '1_45' or '1_44'.
  config.BOOST_VERSION = '1_45'
  
  # Helper function: Given an x, return the tuple (x, x).
  duplicate = lambda x: (x, x)
  
  if platform == 'win32':
    # Set this to your version of Visual Studio that you used to compile Boost Libraries and will use
    # to compile APNS module. (Yes, they must be the same version.) Good values are 'vc100', 'vc9' or 'vc8'.
    config.VISUAL_STUDIO_VERSION = 'vc100'
  
    # These values are tuples. The first element is the ordinary library, the second element is the
    # debug version of the same library.
  
    pythonVerString = '%d%d' % (config.PYTHON_VERSION[0], config.PYTHON_VERSION[1])
    config.PYTHON_LIB = duplicate('python%s' % pythonVerString)
  
    # Use the auto-linking feature of the Microsoft compiler together with the fact that Boost Libraries use it too.
    config.BOOST_RANDOM_LIB = ('', '')
    config.BOOST_PYTHON_LIB = ('', '')
    
    config.GTEST_LIB = ('gtest', 'gtestd')
  
  else:
    pythonVerString = '%d.%d' % (config.PYTHON_VERSION[0], config.PYTHON_VERSION[1])
    config.PYTHON_LIB = duplicate('python%s' % pythonVerString)
  
    config.BOOST_RANDOM_LIB = duplicate('boost_random')
    config.BOOST_PYTHON_LIB = duplicate('boost_python')
  
    config.GTEST_LIB = duplicate('gtest')
  
  # Compiler version
  if platform == 'win32':
    config.COMPILER = None  # Keep default.
    config.RPATH = None     # Makes no sense for MSVS.
  
  else:
    config.COMPILER = '/usr/local/bin/g++46'  # Use G++ 4.6 which is not in base by default.
    config.RPATH = '/usr/local/lib/gcc46/'

  return config
