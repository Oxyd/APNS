#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''cmdline.py -- the command line interface for the program.'''

from interface.search import makeSearch
from interface.fileio import loadBoard, loadSearch, saveSearch
from apnsmod import WinStrategy, OperationController, memoryUsedTotal
import argparse
import sys
import os
import time

KB = 1024         # One kilobyte
MB = 1024 * 1024  # One megabyte

class SaveLoadProgress(OperationController):
  def __init__(self, quiet):
    '''If quiet is True, don't print any progress.'''
    OperationController.__init__(self, 1000)
    self._quiet = quiet
  
  def doUpdate(self):
    if not self._quiet:
      print '{0}/{1} {2}%'.format(self.workDone, self.workTotal, 
                                  int(100.0 * float(self.workDone) / float(self.workTotal)))
      sys.stdout.flush()


def strFromMem(mem):
  '''strFromMem(m) -> str
  
  Convert an integer m representing an amount of memory to a string representing the amount with units attached.
  '''
  
  if mem < KB:      return '%.2f B' % mem
  elif mem < MB:    return '%.2f kB' % (mem / KB)
  else:             return '%.2f MB' % (mem / MB)
    

def main():
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
  
  if args.position is not None:
    try:
      (board, player) = loadBoard(args.position)
      search = makeSearch(board, player, WinStrategy())
    except Exception, e:
      print >> sys.stderr, 'Error loading specified initial position from specified file: {0}'.format(e)
      raise SystemExit(1)
      
  else:
    progress = SaveLoadProgress(args.quiet)
    if not args.quiet: print 'Loading previous search tree from {0}:'.format(args.searchFile)
    
    try:
      search = loadSearch(args.searchFile, progress)
    except KeyboardInterrupt:
      if not args.quiet: print 'Cancelled'
      raise SystemExit(0)
    except Exception, e:
      print >> sys.stderr, 'Error loading specified search tree from specified file: {0}'.format(e)
      raise SystemExit(1)

    if not args.quiet: print 'Done'
  
  
  transTblElements = args.transTblSize * MB / search.sizeOfTransTblElement
  search.useTranspositionTable(transTblElements, args.transTblKeepTime)

  BURST_TIME = 1000  # How long should search bursts be.
  
  start = time.clock()
  timeElapsed = 0
  lastMeasurement = start
  posPerSec = 0
  lastPosCount = 0
  
  interrupted = False
  if not args.quiet: print 'Starting search. Pres Control-C to stop the search at any time.'
  
  while True:
    try:
      if args.timeLimit > 0 and timeElapsed >= args.timeLimit \
         or args.posLimit > 0 and search.positionCount >= args.posLimit \
         or search.finished:
        break
      
      search.run(BURST_TIME)
      
      if not args.quiet:
        print 'Still working:'
        print '  -- {0} seconds elapsed'.format(int(timeElapsed))
        if args.timeLimit:
          print '  -- {0} seconds left'.format(int(args.timeLimit - timeElapsed))
        print '  -- Root vertex PN: {0}'.format(search.root.proofNumber)
        print '  -- Root vertex DN: {0}'.format(search.root.disproofNumber)
        print '  -- {0} Search memory used'.format(strFromMem(memoryUsedTotal()))
        print '  -- {0} unique positions total'.format(search.positionCount)
        print '  -- {0} new positions per second'.format(posPerSec)
        
        if search.getTranspositionTable() is not None:
          tbl = search.getTranspositionTable()
          print '  -- Transposition table:'
          print '    -- Size: {0:.2f} MB'.format(float(tbl.memoryUsage) / MB)
          print '    -- Hits: {0}'.format(tbl.hits)
          print '    -- Misses: {0}'.format(tbl.misses)
      
      now = time.clock()
      timeElapsed = now - start
      
      if now - lastMeasurement >= 1.0:
        posPerSec = search.positionCount - lastPosCount
        lastPosCount = search.positionCount
        lastMeasurement = now
      
    except KeyboardInterrupt:
      interrupted = True
      break
    
  if not args.quiet: 
    print 'Search finished:',
    if search.root.proofNumber == 0:        print 'Root vertex is proved'
    elif search.root.disproofNumber == 0:   print 'Root vertex is disproved'
    elif args.timeLimit > 0 and timeElapsed >= args.timeLimit:
      print 'Time limit exceeded'
    elif args.posLimit > 0 and search.positionCount >= args.posLimit:
      print 'Position limit exceeded'
    elif interrupted:
      print 'User interrupted'
    
    print 'Saving result to {0}'.format(args.destination)
  
  progress = SaveLoadProgress(args.quiet)
  
  try:
    saveSearch(search, args.destination, progress)
  except KeyboardInterrupt:
    if not args.quiet:  print 'Cancelled. No output file will be generated'
    os.remove(args.destination)
    raise SystemExit(0)
  
  if not args.quiet: 
    print 'Done'
    print 'Cleaning up...'
  
  del search
  
  if not args.quiet: print 'Finished'

if __name__ == '__main__':
  main()