#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''cmdline.py -- the command line interface for the program.'''

from interface.search import makeSearch
from interface.fileio import loadBoard, loadSearch, saveSearch
from interface.controller import Controller, SearchParameters
import apnsmod
import argparse
import sys
import os
import time
import signal

KB = 1024         # One kilobyte
MB = 1024 * 1024  # One megabyte

class SaveLoadProgress(apnsmod.OperationController):
  def __init__(self, quiet):
    '''If quiet is True, don't print any progress.'''
    apnsmod.OperationController.__init__(self, 1000)
    self._quiet = quiet

  def doUpdate(self):
    pass

def strFromMem(mem):
  '''strFromMem(m) -> str

  Convert an integer m representing an amount of memory to a string representing the amount with units attached.
  '''

  if mem < KB:      return '%.2f B' % mem
  elif mem < MB:    return '%.2f kB' % (mem / KB)
  else:             return '%.2f MB' % (mem / MB)


def strFromNum(num):
  '''Get the string representation of a proof or disproof number.'''

  if num < apnsmod.Vertex.infty:
    return str(num)
  else:
    return 'inf'


def main():
  is64Bit = sys.maxsize > 2**32  # Trick straight from the docs.

  parser = argparse.ArgumentParser(description='Execute the Proof-Number Search algorithm and save the result')
  parser.add_argument('-p', '--position', type=str, dest='position',
                      help='File containing the initial search position')
  parser.add_argument('-s', '--search', type=str, dest='searchFile',
                      help='File containing the results of a previous search that should be resumed')
  parser.add_argument('-d', '--destination', type=str, required=True,
                      help='Name of the output file')
  parser.add_argument('-t', '--time', type=int, default=60, metavar='time limit', dest='timeLimit',
                      help='Maximum running time of the algorithm, excluding any I/O operations, in seconds')
  parser.add_argument('-n', '--positions', type=int, default=0, metavar='position limit', dest='posLimit',
                      help='Maximum number of unique positions examined')
  parser.add_argument('-m', '--memory', type=int, default=0 if is64Bit else 1500, metavar='memory limit', dest='memLimit',
                      help='Maximum amount of memory to be used by the computation, in megabytes')
  parser.add_argument('-r', '--trans-tbl-size', type=int, default=32, metavar='trans tbl size', dest='transTblSize',
                      help='Size of the transposition table to use, in megabytes. If set to 0, don\'t use transposition table'
                      'at all')
  parser.add_argument('-k', '--trans-tbl-keep-time', type=int, default=16, metavar='trans tbl keep time', dest='transTblKeepTime',
                      help='How long should elements be kept in the transposition table before they can be replaced with newer'
                      'entries')
  parser.add_argument('-q', '--quiet', const=True, default=False, action='store_const', dest='quiet',
                      help='Don\'t print any messages to standard output.')
  args = parser.parse_args()

  if args.searchFile is None and args.position is None:
    print >> sys.stderr, 'Error: Either initial position os previous search must be specified'
    raise SystemExit(1)

  if args.searchFile is not None and args.position is not None:
    print >> sys.stderr, 'Error: Initial position and previous search can\'t be specified at the same time.'
    raise SystemExit(1)

  if args.transTblSize < 0:
    print >> sys.stderr, 'Error: Size of transposition table must be a nonnegative integer'
    raise SystemExit(1)
  if args.transTblKeepTime < 0:
    print >> sys.stderr, 'Error: Transposition table keep time must be a nonnegative integer'
    raise SystemExit(1)
  if args.timeLimit < 0:
    print >> sys.stderr, 'Error: Time limit must be a nonnegative integer'
    raise SystemExit(1)
  if args.posLimit < 0:
    print >> sys.stderr, 'Error: Position limit must be a nonnegative integer'
    raise SystemExit(1)
  if args.memLimit < 0:
    print >> sys.stderr, 'Error: Memory limit must be a nonnegative integer'
    raise SystemExit(1)

  params = SearchParameters()
  params.timeLimit = args.timeLimit
  params.positionLimit = args.posLimit
  params.memoryLimit = args.memLimit
  params.transTblSize = args.transTblSize
  params.transTblKeepTime = args.transTblKeepTime

  controller = Controller()
  controller.searchParameters = params

  class InterruptHandler:
    def __init__(self):
      self.interrupted = False
      signal.signal(signal.SIGINT, self.handle)

    def handle(self, signum, frame):
      self.interrupted = True
      controller.cancel()

    def reset(self):
      self.interrupted = False

  interruptHandler = InterruptHandler()

  def show(s):
    if not args.quiet: print s

  def cancelCallback(ctrl):
    if interruptHandler.interrupted:
      ctrl.cancel()

  controller.loadGameCallbacks.add(cancelCallback)
  controller.saveGameCallbacks.add(cancelCallback)

  if args.position is not None:
    try:
      controller.newGame(args.position)
    except Exception, e:
      print >> sys.stderr, 'Error loading specified initial position from specified file: {0}'.format(e)
      raise SystemExit(1)

  else:
    show('Loading previous search tree from {0}:'.format(args.searchFile))

    try:
      controller.loadGame(args.searchFile)
    except Exception, e:
      print >> sys.stderr, 'Error loading specified search tree from specified file: {0}'.format(e)
      raise SystemExit(1)

    if not interruptHandler.interrupted:
      show('Done')
    else:
      show('Cancelled')
      raise SystemExit(0)

  transTblElements = args.transTblSize * MB / apnsmod.TranspositionTable.sizeOfElement
  #search.useTranspositionTable(transTblElements, args.transTblKeepTime)

  class ProgressPrinter:
    def __init__(self):
      self.lastMeasurement = time.clock()
      self.posPerSec = 0
      self.lastPosCount = controller.positionCount

    def __call__(self, ctrl, progress):
      show('Still working:')
      show('  -- {0} seconds elapsed'.format(int(progress.timeElapsed)))
      if progress.timeLeft:
        show('  -- {0} seconds left'.format(int(progress.timeLeft)))
      show('  -- Root vertex PN: {0}'.format(strFromNum(progress.rootPN)))
      show('  -- Root vertex DN: {0}'.format(strFromNum(progress.rootDN)))
      #show('  -- {0} Search memory used'.format(strFromMem(memoryUsedTotal())))
      show('  -- {0} unique positions total'.format(progress.positionCount))
      show('  -- {0} new positions per second'.format(self.posPerSec))

      if progress.transTblSize:
        show('  -- Transposition table:')
        show('    -- Size: {0:.2f} MB'.format(float(progress.transTblSize) / MB))
        show('    -- Hits: {0}'.format(progress.transTblHits))
        show('    -- Misses: {0}'.format(progress.transTblMisses))

      if time.clock() - self.lastMeasurement >= 1.0:
        self.posPerSec = progress.positionCount - self.lastPosCount
        self.lastPosCount = progress.positionCount

  controller.searchProgressCallbacks.add(ProgressPrinter())
  show('Starting search. Pres Control-C to stop the search at any time.')

  controller.runSearch(burst=1000)

  if not args.quiet:
    print 'Search finished:',
    if controller.root.proofNumber == 0:        print 'Root vertex is proved'
    elif controller.root.disproofNumber == 0:   print 'Root vertex is disproved'
    elif params.timeLimit > 0 and controller.stats.timeElapsed >= params.timeLimit:
      print 'Time limit exceeded'
    elif params.positionLimit > 0 and controller.stats.positionCount >= params.positionLimit:
      print 'Position limit exceeded'
    elif params.memoryLimit > 0 and (memoryUsedTotal() / (1024 ** 2)) >= params.memoryLimit:
      print 'Memory limit exceeded'
    elif interruptHandler.interrupted:
      print 'User interrupted'
      interruptHandler.reset()

  show('Saving result to {0}'.format(args.destination))
  controller.saveGame(args.destination)
  if not interruptHandler.interrupted:
    show('Done')
  else:
    show('Cancelled')

if __name__ == '__main__':
  main()
