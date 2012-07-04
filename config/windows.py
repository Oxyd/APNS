# config/windows.py -- configuration values for building on Windows systems.

def config(conf, bits, debug, profile):
  if bits == 64:
    boostBase   = 'C:/Users/Oxyd/Development/boost_1_48_0/'
    pythonBase  = 'C:/Python27/'
  else:
    boostBase   = 'C:/Users/Oxyd/Development/boost_1_48_0-32/'
    pythonBase  = 'C:/Python27-32/'

  conf['win32'] = {
    'default-toolchain':  'msvc',

    'msvc': {
      'target-arch': 'x86_64' if bits == 64 else 'x86',

      'compile-flags': [
        '/EHsc',                    # Enable C++ exception handling.
        '/Zc:forScope',             # Standard C++ scoping rules.                                                                                                                                                               
        '/wd4224',                  # Disable an annoying warning. (I believe MSVS is being wrong here.)                                                                                                                        
        '/wd4180',                  # Disable the "C4180: qualifier applied to function type has no meaning; ignored" warning.                                                                                    
                                    # It looks like MSVC likes to warn about this even though it really shouldn't.
        '/wd4005',                  # Disable C4005: Macro redefinition

        '/Ot' if not debug else '', # Favor code speed.                                                                                                                                                                               
        '/Ox' if not debug else '', # Maximum optimisations.

        # Choose the apropriate version of the run-time library -- debugging or release one.
        '/MT' if not debug else '/MTd'
      ],
      'defines': [
        'NOMINMAX',                 # MSVS defines min and max names as macros otherwise. Evil, sad, but true.                                                                                                       
        '_SCL_SECURE_NO_WARNINGS',  # Reduce the number of warnings from the standard library implementation.                                                                                                        
        'BOOST_PYTHON_STATIC_LIB',  # I want to link against Boost.Python statically on Windows.                                                                                                                     
        'BOOST_SP_DISABLE_THREADS',
        '_MBCS',
        'NDEBUG' if not debug else '',

        # Boost libraries are apparantely compiled with this flag in debug mode. Client app needs to have the same flag then.        
        '_ITERATOR_DEBUG_LEVEL=2' if debug else '',
      ],

      'includedirs': {
        'boost-python':     boostBase,
        'boost-random':     boostBase,
        'boost-filesystem': boostBase,
        'boost-system':     boostBase,
        'python':           pythonBase + 'include/',
        'gtest':            'C:/Users/Oxyd/Development/gtest-1.5.0/include'
      },
      'libdirs': {
        'boost-python':     boostBase + 'stage/lib/',
        'boost-random':     boostBase + 'stage/lib/',
        'boost-filesystem': boostBase + 'stage/lib/',
        'boost-system':     boostBase + 'stage/lib/',
        'python':           pythonBase + 'libs/',
        'gtest':            'C:/Users/Oxyd/Development/gtest-1.5.0/lib'
      },
      'libs': {
        'python':           'python27',
        'boost-python':     '',  # Use the auto-linking feature of MSVC.
        'boost-random':     '',
        'boost-filesystem': '',
        'boost-system':     '',
        'gtest':            'gtest' if not debug else 'gtestd'
      },
      'extra':            lambda env: _extraSetup(env, bits, debug)
    }
  }

def _extraSetup(env, bits, debug):
  # Set the shared library suffix to .pyd, which is the Python extension suffix for Windows systems.
  env.Replace(SHLIBSUFFIX='.pyd')
  env.Replace(LIBSUFFIXES=['.pyd'])

  # Don't install the import library to the top-level directory.
  env.Replace(no_import_lib=1)

  if debug:
    env.Replace(PDB='${TARGET.base}.pdb')
