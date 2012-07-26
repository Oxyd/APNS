APNS is an endgame analyzer for Arimaa. (See http://arimaa.com/) This is a part
of my bachelor thesis at the Faculty of Mathematics and Physics at Charles
University of Prague. 

To build this program, you need the scons tool, development files for the Boost
Collection and development files for Python 2.6 or 2.7. Google Test is also
required if you want to run unit tests. The following targets are supported:

  scons               -- makes an optimized build of the program
  scons debug=1       -- makes a debug build
  scons profile=1     -- makes a profiling build
  scons debug=1 tests -- runs all included unit tests

The result of the build is a Python extension module. The program may then be
run either as ./batch.py in command-line only mode, or as ./gui.pyw in GUI mode.
The latter requires Tkinter to be installed on the host system.

For Windows users, precompiled binary archives are available in the Downloads
section. Python 2.6 or 2.7 is still required to run the program.
