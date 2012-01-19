# SConstruct -- main build script for the project.

import sys
stderr = sys.stderr
#from build_config import buildConfig
from os import popen
from glob import glob

bits = int(ARGUMENTS.get('bits', 64))
if bits not in (32, 64):
  print >> stderr, "bits variable must be set either to 32 or 64"
  raise SystemExit(1)

debug = int(ARGUMENTS.get('debug', 0))

config = {}
for c in glob('config/*.py'):
  print 'SConstruct: Reading platform configuration file', c
  mod = dict()
  execfile(c, mod)
  mod['config'](config, bits, debug)

platform = ARGUMENTS.get('platform', sys.platform)
toolchain = ARGUMENTS.get('toolchain', config[platform]['default-toolchain'])

if platform not in config:
  print >> stderr, "No configuration could be found for platform", platform
  raise SystemExit(1)

if toolchain not in config[platform]:
  print >> stderr, "No configuration could be found for toolchain", toolchain, "for this platform"
  raise SystemExit(1)

conf = config[platform][toolchain]

# Use pdflatex on Windows and let the user build the documentation manually on FreeBSD.
if platform == 'win32':
  latexCmd = 'pdflatex'
  latexCmd = ARGUMENTS.get('latexcmd', latexCmd)

  doc = SConscript('doc/SConscript', exports=['latexCmd'])

  Alias('doc', doc)

# Make a build environment here.
env = Environment()
env.Append(CPPPATH=conf['includedirs'])
env.Append(LIBPATH=conf['libdirs'])
env.Append(LIBS=[conf['libs']['python'], conf['libs']['boost-python']])
if 'compiler' in conf:      env.Replace(CXX=conf['compiler'])
if 'defines' in conf:       env.Append(CPPDEFINES=conf['defines'])
if 'compile-flags' in conf: env.Append(CCFLAGS=conf['compile-flags'])

# Process any extra configuration that needs to be done for this platform.
if 'extra' in conf: conf['extra'](env)

Export('debug', 'bits', 'env', 'toolchain', config=config[platform][toolchain])

subTargets = SConscript('apns_module/SConscript')
apnslib = subTargets['apnslib']

install = Install('.', apnslib)  # Copy the Python extension module to the toplevel directory.
Default(install)  # This merely ensures that the 'install' target gets invoked on 'scons'.

# Alias all sub-targets here so that this script can be invoked using the more convenient syntax of 'scons board_test' instead
# of 'scons apns_module/board_test'.

for name, target in subTargets.items():
  Alias(name, target)

# Windows binary distribution targets.

ver = popen('git describe').read().rstrip()
distribName = 'apns-windows-{0}bit-{2}{1}.zip'.format(bits, '-debug' if debug else '', ver)
distr = Zip(distribName,
  [ 'apnsmod.pyd',
    'batch.py',
    'gui.pyw',
    'interface/',
    'doc/doc.pdf',
    'example-positions/' ])

Alias('distrib', distr)
