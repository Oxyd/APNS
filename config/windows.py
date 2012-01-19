# config/windows.py -- configuration values for building on Windows systems.

def config(conf, bits, debug):
  if bits == 64:
    boostBase   = 'C:/Users/Oxyd/Development/boost_1_48_0/'
    pythonBase  = 'C:/Users/Python27/'
  else:
    boostBase   = 'C:/Users/Oxyd/Development/boost_1_48_0-32/'
    pythonBase  = 'C:/Users/Python27-32/'

  conf['win32'] = {
    'default-toolchain':  'mingw',
    'mingw': {
      'compile-flags': [
        '-Wall',
        '-Wextra',
        '-ansi',
        '-pedantic',
        '-O3' if not debug else '',
        '-ggdb' if debug else '',
        '-fstack-protector-all' if debug else ''
      ],

      'defines': [
        'NDEBUG' if not debug else '',
        '_GLIBCXX_DEBUG' if debug else '',
        '_GLIBCXX_DEBUG_PEDANTIC' if debug else ''
      ],

      'includedirs':      [boostBase,
                           pythonBase + 'include/',
                           'C:/Users/Oxyd/Development/gtest-1.5.0/include'],
      'libdirs':          [boostBase + 'stage/lib/',
                           pythonBase + 'libs/',
                           'C:/Users/Oxyd/Development/gtest-1.5.0/lib'],
      'libs': {
        'python':         'python27',
        'boost-python':   'boost_python-mgw64-mt%s-1_48.a' % ('-d' if debug else ''),
        'gtest':          'gtest' if not debug else 'gtestd'
      },
      'extra':            _setupPrefix
    }
  }

def _setupPrefix(env):
  # Set the shared library suffix to .pyd, which is the Python extension suffix for Windows systems.
  env.Replace(SHLIBSUFFIX='.pyd')
  env.Replace(LIBSUFFIXES=['.pyd'])

  # Don't install the import library to the top-level directory.
  env.Replace(no_import_lib=1)
