# config/linux.py -- configuration values for building on Linux systems.

def config(conf, bits, debug, profile):
  conf['linux2'] = {
    'default-toolchain':  'gcc',
    'gcc': {
      'compile-flags': [
        '-pipe',
        '-Wall',
        '-Wextra',
        '-std=c++03',
        '-pedantic',
        '-O3' if not debug else '',
        '-ggdb' if debug or profile else '',
        '-fstack-protector-all' if debug else ''
      ],
      'link-flags': [
      ],
      'defines': [
        'POSIX',
        'BOOST_SP_DISABLE_THREADS',
        'NDEBUG' if not debug else '',
        '_GLIBCXX_DEBUG' if debug else '',
        '_GLIBCXX_DEBUG_PEDANTIC' if debug else ''
      ],
      'includedirs': {
        'boost-python':     '',  # These two are already found in the standard include locations.
        'boost-random':     '',
        'boost-filesystem': '',
        'boost-system':     '',
        'boost-timer':      '',
        'boost-chrono':     '',
        'gtest':            '',
        'python':           '/usr/include/python2.7/'
      },
      'libdirs': {
        'boost-python':     '',  # Same as above.
        'boost-random':     '',  # Same as above.
        'boost-filesystem': '',
        'boost-system':     '',
        'boost-timer':      '',
        'boost-chrono':     '',
        'gtest':            '',
        'python':           '/usr/lib/python2.7/'
      },
      'libs': {
        'python':           'python2.7',
        'boost-python':     'boost_python',
        'boost-random':     'boost_random',
        'boost-filesystem': 'boost_filesystem',
        'boost-system':     'boost_system',
        'boost-timer':      'boost_timer',
        'boost-chrono':     'boost_chrono',
        'rt':               'rt',
        'gtest':            'gtest',
      },
      'extra':            _setupPrefix
    },

    'clang': {
      'compiler': 'clang++'

      # The rest is the same as for GCC.
    }
  }

  for key, value in conf['linux2']['gcc'].iteritems():
    conf['linux2']['clang'][key] = value

def _setupPrefix(env):
  env.Replace(SHLIBPREFIX='')  # Do *not* use the 'lib' prefix on *nix systems.
