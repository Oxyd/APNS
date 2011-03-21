# SConstruct -- main build script for the project.

from sys import platform, stderr
from build_config import buildConfig

bits = int(ARGUMENTS.get('bits', 64))

if bits not in (32, 64):
  print >> stderr, "bits variable must be set either to 32 or 64"
  raise SystemExit(1)

config = buildConfig(platform, bits)
debug = int(ARGUMENTS.get('debug', 0))

subTargets = SConscript('apns_module/SConscript', exports=['config', 'debug', 'bits'])

apnslib = subTargets['apnslib']

install = Install('.', apnslib)  # Copy the Python extension module to the toplevel directory.
Default(install)  # This merely ensures that the 'install' target gets invoked on 'scons'.

# Alias all sub-targets here so that this script can be invoked using the more convenient syntax of 'scons board_test' instead
# of 'scons apns_module/board_test'.

for name, target in subTargets.items():
  Alias(name, target)

# Windows binary distribution targets.

distribName = 'apns-windows-{0}bit{1}.zip'.format(bits, '-debug' if debug else '')
distr = Zip(distribName,
  [ 'apnsmod.pyd',
    'batch.py',
    'gui.pyw',
    'interface/',
    'doc/',
    'example-positions/' ])

Alias('distrib', distr)
