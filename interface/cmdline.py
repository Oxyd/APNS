#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''cmdline.py -- the command line interface for the program.'''

from interface.controller import Controller, SearchParameters
import apnsmod
import argparse
import sys
import time
import signal

KB = 1024         # One kilobyte
MB = 1024 * 1024  # One megabyte

EXIT_FINISHED = 0
EXIT_LIMIT = 1
EXIT_ERROR = 2
EXIT_CANCEL = 3

class SaveLoadProgress(apnsmod.OperationController):
  def __init__(self, quiet):
    '''If quiet is True, don't print any progress.'''
    apnsmod.OperationController.__init__(self, 1000)
    self._quiet = quiet

  def doUpdate(self):
    pass

def strFromMem(mem):
  '''strFromMem(m) -> str

  Convert an integer m representing an amount of memory to a string
  representing the amount with units attached.
  '''

  if mem < KB:
    return '{0}'.format(mem)
  elif mem < MB:
    return '{0:.2f} kB'.format(mem / float(1024))
  else:
    return '{0:.2f} MB'.format(mem / float(1024 * 1024))


def strFromNum(num):
  '''Get the string representation of a proof or disproof number.'''

  if num < apnsmod.Vertex.infty:
    return str(num)
  else:
    return 'inf'


start    = None  # To be set later by stateCallback
startCpu = None  # Ditto.

def main():
  is64Bit = sys.maxsize > 2**32  # Trick straight from the docs.

  parser = argparse.ArgumentParser(
    description='Execute the Proof-Number Search algorithm and save the result'
  )

  parser.add_argument('-p', '--position', type=str, dest='position',
                      help='File containing the initial search position')
  parser.add_argument('-s', '--search', type=str, dest='searchFile',
                      help='File containing the results of a previous search '+
                           'that should be resumed')
  parser.add_argument('-d', '--destination', type=str, required=False,
                      help='Name of the output file. If you don\'t specify ' +
                      'this, the search tree won\'t be saved anywhere.')
  parser.add_argument('-a', '--algorithm', type=str, default='pns',
                      metavar='algorithm', dest='algo',
                      help='Algorithm to use. Valid values are {0}'.format(
                          ', '.join(apnsmod.algos.keys()[:-1]) + 
                          ', and ' + apnsmod.algos.keys()[-1]
                      ))
  parser.add_argument('-t', '--time', type=int, default=0,
                      metavar='time limit', dest='timeLimit',
                      help='Maximum running time of the algorithm, ' +
                           'excluding any I/O operations, in seconds')
  parser.add_argument('-n', '--positions', type=int, default=0,
                      metavar='position limit', dest='posLimit',
                      help='Maximum number of unique positions examined')
  parser.add_argument('-m', '--memory', type=int,
                      default=0 if is64Bit else 1500, metavar='memory limit',
                      dest='memLimit',
                      help='Maximum amount of memory to be used by the ' +
                           'computation, in megabytes')
  parser.add_argument('-r', '--trans-tbl-size', type=int, default=32,
                      metavar='trans tbl size', dest='transTblSize',
                      help='Size of the transposition table to use, in ' +
                           'megabytes. If set to 0, don\'t use ' +
                           'transposition table'
                      'at all')
  parser.add_argument('-o', '--proof-tbl-size', type=int, default=32, 
                      metavar='proof tbl size', dest='proofTblSize',
                      help='Size of the proof table to use, in megabytes. ' +
                           'If set to 0, don\'t use proof table')
  parser.add_argument('-k', '--killer-count', type=int, default=2, dest='killerCount',
                      help='Number of killer steps remembered for each level.')
  #parser.add_argument('-M', '--move-cache-size', type=int, default=32,
  #                    metavar='move cache size', dest='moveCacheSize',
  #                    help='Size of the move cache')
  parser.add_argument('-g', '--gc-low', type=int, default=3000000,
                      metavar='gc low', dest='gcLow',
                      help='Garbage collector low threshold.')
  parser.add_argument('-G', '--gc-high', type=int, default=5000000,
                      metavar='gc high', dest='gcHigh',
                      help='Garbage collector high threshold.')
  parser.add_argument('-q', '--quiet', const=True, default=False,
                      action='store_const', dest='quiet',
                      help='Don\'t print any messages to standard output.')
  parser.add_argument('-Q', '--no-progress', const=True, default=False,
                      action='store_const', dest='noProgress',
                      help='Don\'t print any progress information, only ' +
                           'the summary at the end.')
  parser.add_argument('-l', '--log', dest='logFilename',
                      nargs='?', const='',
                      help='Save execution log into given file. Specify -l ' +
                           'alone to print the log to standard output')
  parser.add_argument('-w', '--answer', dest='answer', const=True, default=False,
                      action='store_const',
                      help='If the root is proved, print the suggested move.')
  args = parser.parse_args()

  if args.searchFile is None and args.position is None:
    print >> sys.stderr, \
        'Error: Either initial position or previous search must be specified'
    parser.print_usage()
    raise SystemExit(EXIT_ERROR)

  if args.searchFile is not None and args.position is not None:
    print >> sys.stderr, \
        'Error: Initial position and previous search can\'t be specified ' + \
        'at the same time.'
    raise SystemExit(EXIT_ERROR)

  if args.algo not in apnsmod.algos:
    print >> sys.stderr, \
        'Error: Algorithm must be one of', ', '.join(apnsmod.algos)
    raise SystemExit(EXIT_ERROR)

  def checkNum(value, name):
    if value < 0:
      print >> sys.stderr, \
          'Error: {0} must be a nonnegative integer'.format(name)
      raise SystemExit(EXIT_ERROR)

  checkNum(args.transTblSize, 'Size of transposition table')
  checkNum(args.proofTblSize, 'Size of proof table')
  checkNum(args.timeLimit, 'Time limit')
  checkNum(args.posLimit, 'Position limit')
  checkNum(args.memLimit, 'Memory limit')
  checkNum(args.gcLow, 'Garbage collector low threshold')
  checkNum(args.gcHigh, 'Garbage collector high threshold')
  checkNum(args.killerCount, 'Number of killers')

  params = SearchParameters()
  params.algo = args.algo
  params.timeLimit = args.timeLimit
  params.positionLimit = args.posLimit
  params.memoryLimit = args.memLimit
  params.transTblSize = args.transTblSize
  params.proofTblSize = args.proofTblSize
  params.gcLow = args.gcLow
  params.gcHigh = args.gcHigh
  params.logFilename = args.logFilename
  params.killerCount = args.killerCount

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

  def stateCallback(ctrl, state):
    if state == Controller.State.ALLOCATING:
      show('Allocating transposition and proof tables...')
    elif state == Controller.State.SEARCHING:
      show('... Done. Commencing search.')

      global start, startCpu

      start = time.time()
      startCpu = time.clock()

  controller.stateCallbacks.add(stateCallback)
  controller.loadGameCallbacks.add(cancelCallback)
  controller.saveGameCallbacks.add(cancelCallback)

  if args.noProgress: args.quiet = True

  if args.position is not None:
    try:
      controller.newGame(args.position)
    except Exception, e:
      print >> sys.stderr, \
          'Error loading specified initial position from specified ' + \
          'file: {0}'.format(e)
      raise SystemExit(EXIT_ERROR)

  else:
    show('Loading previous search tree from {0}:'.format(args.searchFile))

    try:
      controller.loadGame(args.searchFile)
    except Exception, e:
      print >> sys.stderr, \
        'Error loading specified search tree from specified ' + \
        'file: {0}'.format(e)
      raise SystemExit(EXIT_ERROR)

    if not interruptHandler.interrupted:
      show('Done')
    else:
      show('Cancelled')
      raise SystemExit(EXIT_CANCEL)

  def printProgress(ctrl, progress, summary=False):
    if summary:
      show('Search summary:')
    else:
      show('Still working:')

    show('  -- {0} seconds elapsed'.format(int(progress.timeElapsed)))
    if progress.timeLeft and not summary:
      show('  -- {0} seconds left'.format(int(progress.timeLeft)))
    show('  -- Root vertex PN: {0}'.format(strFromNum(progress.rootPN)))
    show('  -- Root vertex DN: {0}'.format(strFromNum(progress.rootDN)))
    show('  -- {0} tree memory used'.format(strFromMem(progress.memUsed)))
    show('  -- {0} unique positions total'.format(progress.positionCount))
    show('  -- {0} new positions per second'.format(
      int(progress.positionsPerSecond))
    )

    if progress.transTblSize:
      show('  -- Transposition table:')
      show('    -- Size:   {0:.2f} MB'.format(
        float(progress.transTblSize) / MB)
      )
      show('    -- Hits:   {0}'.format(progress.transTblHits))
      show('    -- Misses: {0}'.format(progress.transTblMisses))

    if progress.proofTblSize:
      show('  -- Proof table:')
      show('    -- Size:   {0:.2f} MB'.format(
        float(progress.proofTblSize) / MB)
      )
      show('    -- Hits:   {0}'.format(progress.proofTblHits))
      show('    -- Misses: {0}'.format(progress.proofTblMisses))

    show('  -- Total killer count: {0}'.format(progress.killerCount))

  controller.searchProgressCallbacks.add(printProgress)
  show('Starting search. Pres Control-C to stop the search at any time.')

  try:
    controller.runSearch(burst=1000)
  except RuntimeError, e:
    print >> sys.stderr, \
      'Internal error has occured; this is likely a bug.\n\n' + str(e)
    raise SystemExit(EXIT_ERROR)
  except MemoryError:
    print >> sys.stderr, \
      'Error: The program ran out of memory while trying to expand the tree.'
    raise SystemExit(EXIT_ERROR)

  end = time.time()
  endCpu = time.clock()

  exitStatus = None

  if not args.quiet or args.noProgress:
    print 'Search finished:',
    if controller.root.proofNumber == 0:
      print 'Root vertex is proved'
      exitStatus = EXIT_FINISHED
    elif controller.root.disproofNumber == 0:
      print 'Root vertex is disproved'
      exitStatus = EXIT_FINISHED
    elif params.timeLimit > 0 and \
        controller.stats.timeElapsed >= params.timeLimit:
      print 'Time limit exceeded'
      exitStatus = EXIT_LIMIT
    elif params.positionLimit > 0 and \
        controller.stats.positionCount >= params.positionLimit:
      print 'Position limit exceeded'
      exitStatus = EXIT_LIMIT
    elif params.memoryLimit > 0 and \
        (controller.root.subtreeBytes / (1024 ** 2)) >= params.memoryLimit:
      print 'Memory limit exceeded'
      exitStatus = EXIT_LIMIT
    elif interruptHandler.interrupted:
      print 'Interrupted'
      interruptHandler.reset()
      exitStatus = EXIT_CANCEL

    if args.noProgress:
      args.quiet = False
      printProgress(controller, controller.stats, summary=True)
      args.quiet = True

  if not args.quiet or args.noProgress:
    print 'Elapsed: Real time: {0:.2f} seconds, CPU time: {1:.2f} seconds'.format(
      end - start, endCpu - startCpu
    )

  if args.answer and controller.root.proofNumber == 0:
    print 'Best move:',

    v = controller.root
    while v is not None:
      if v.step:
        print v.step.toString(),

      if v.type_ == apnsmod.Vertex.Type.or_:
        v = apnsmod.bestSuccessor(v)
      else:
        v = None

  if args.destination is not None:
    show('Saving result to {0}'.format(args.destination))
    controller.saveGame(args.destination)
    if not interruptHandler.interrupted:
      show('Done')
    else:
      show('Cancelled')
  
  sys.exit(exitStatus)

if __name__ == '__main__':
  main()
