# config/linux.py -- configuration values for building on Linux systems.

def config(conf, bits, debug):
  conf['linux2'] = {
    'default-toolchain':  'gcc',
    'gcc': {
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
      'includedirs':      ['/usr/include/', '/usr/include/python2.7/'],
      'libdirs':          ['/usr/lib/', '/usr/lib/python2.7/'],
      'libs': {
        'python':         'python2.7',
        'boost-python':   'boost_python',
        'gtest':          'gtest'
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
