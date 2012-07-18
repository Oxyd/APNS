# -*- coding: utf-8 -*-

import apnsmod
import time
import re
import sys

class _Callbacks(object):
  def __init__(self):
    object.__init__(self)
    self._callbacks = list()

  def add(self, callback):
    self._callbacks.append(callback)

  def remove(self, callback):
    self._callbacks.remove(callback)

  def call(self, *args, **kwargs):
    for c in self._callbacks:
      c(*args, **kwargs)


def loadBoard(filename):
  '''loadBoard(filename) -> (Board, Color)

  Load board position from file described by filename. Return the new board and
  color of the player to take the next turn.

  The expected file format is the format described at
  http://arimaa.com/arimaa/learn/notation.html .
  '''

  # Only take nonempty lines.
  lines = filter(lambda line: len(line) > 0, file(filename, 'r').readlines())

  if len(lines) >= 12:
    # First line needs to indicate turn number and player.
    match = re.match(r'\d+([gs])', lines[0])
    if match is None:
      raise RuntimeError(
        'Bad input file format: Expected turn and player on first line'
      )

    player = { 'g': apnsmod.Piece.Color.gold,
               's': apnsmod.Piece.Color.silver }[match.group(1)]

    # Last nonempty line indicates the labelling of the columns.
    lastLine = lines[len(lines) - 1]
    columns = dict()

    for charIndex in xrange(0, len(lastLine)):
      if lastLine[charIndex] in 'abcdefgh':
        if lastLine[charIndex] not in columns:
          columns[lastLine[charIndex]] = charIndex
        else:
          raise RuntimeError('Bad input file format: Invalid column labelling')

      elif lastLine[charIndex] in (' ', '\n', '\r'):
        pass  # Space and newline are okay, just skip them.
      else:
        raise RuntimeError(
          'Bad input file format: Invalid column specification'
        )

    if len(columns) != 8:
      raise RuntimeError(
        'Bad input file format: Expected exactly eight columns'
      )

    rows = dict()
    for lineIndex in xrange(1, len(lines) - 1):
      m = re.match(r'^(\d)\|.*\|', lines[lineIndex])
      if m is not None:
        rows[int(m.group(1))] = lines[lineIndex]
      elif re.match(r'^ *\+-+\+ *', lines[lineIndex]) is not None:
        # Upper or lower border. Skip it.
        pass
      else:
        raise RuntimeError('Bad input file format: Invalid row')

    if (len(rows)) != 8:
      raise RuntimeError('Bad input file format: Expected exactly eight rows')

    board = apnsmod.Board()
    for row in xrange(1, 9):
      for column in 'abcdefgh':
        pieceLetter = rows[row][columns[column]]

        if pieceLetter not in ('x', 'X', ' '):
          piece = apnsmod.pieceFromLetter(pieceLetter)
          board.put(apnsmod.Position(row, column), piece)

    return (board, player)

  else:
    raise RuntimeError('Bad input file format: too few rows')


def saveBoard(board, moveNumber, player, filename):
  '''saveBoard(Board, int, Color, str) -> None

  Save the given board to a file with given name. The 'player' parameter
  specifies the player to move in next turn; the 'stepNumber' parameter
  specifies which move it is.

  The output format is the one described at
  http://arimaa.com/arimaa/learn/notation.html .

  If filename is None, print to the standard output.
  '''

  assert(board is not None)
  assert(player in ('g', 's'))

  output = [ bytearray('%d%s' % (moveNumber, player)),
             bytearray(' +-----------------+'),
             bytearray('8|                 |'),
             bytearray('7|                 |'),
             bytearray('6|     x     x     |'),
             bytearray('5|                 |'),
             bytearray('4|                 |'),
             bytearray('3|     x     x     |'),
             bytearray('2|                 |'),
             bytearray('1|                 |'),
             bytearray(' +-----------------+'),
             bytearray('   a b c d e f g h') ]

  for (position, piece) in board.pieces:
    outputLine = 2 + (8 - position.row)
    outputColumn = 3 + 2 * (ord(position.column) - ord('a'))

    output[outputLine][outputColumn] = apnsmod.letterFromPiece(piece)

  if filename is not None:
    out = file(filename, 'w')
  else:
    out = sys.stdout

  for line in output:
    out.write(str(line) + '\n')

  if filename is not None:
    out.close()



class SearchParameters:
  def __init__(self):
    self.algo           = None
    self.timeLimit      = None
    self.positionLimit  = None
    self.memoryLimit    = None
    self.transTblSize   = None
    self.proofTblSize   = None
    #self.moveCacheSize  = 32
    self.killerCount    = 2
    self.gcHigh         = 5000000
    self.gcLow          = 3000000
    self.heurEval       = False
    self.dynWidening    = apnsmod.DynWidening.none
    self.dynWideningPar = 0
    self.logFilename    = None


class SearchProgress:
  def __init__(self):
    self.timeElapsed        = None
    self.timeLeft           = None
    self.memUsed            = None
    self.rootPN             = None
    self.rootDN             = None
    self.positionCount      = None
    self.positionsPerSecond = None
    self.transTblSize       = None
    self.transTblHits       = None
    self.transTblMisses     = None
    self.proofTblSize       = None
    self.proofTblHits       = None
    self.proofTblMisses     = None
    self.killerCount        = None

MB = 1024 * 1024

class Controller(object):
  _MS_BURST_TIME = 100

  class _OpCtrl(apnsmod.OperationController):
    def __init__(self, ctrl, callbacks):
      apnsmod.OperationController.__init__(self, 100)
      self.ctrl = ctrl
      self.callbacks = callbacks

    def doUpdate(self):
      self.callbacks.call(self.ctrl)
      if self.ctrl._cancel:
        self.requestStop()


  class State:
    INACTIVE, SEARCHING, ALLOCATING = range(3)


  def __init__(self):
    object.__init__(self)
    self.dropGame()

    self.searchParameters = SearchParameters()

    self.searchProgressCallbacks = _Callbacks()
    self.stateCallbacks = _Callbacks()
    self.loadGameCallbacks = _Callbacks()
    self.saveGameCallbacks = _Callbacks()

    self._state = Controller.State.INACTIVE

  state = property(lambda self: self._state)
  gameLoaded = property(lambda self: self._game is not None)
  root = property(lambda self: self._game.root if self._game else None)
  initialState = property(
    lambda self: self._game.initialState if self._game else None
  )
  attacker = property(lambda self: self._game.attacker if self._game else None)

  def newGame(self, *args):
    '''Load a new game from an initial board file or from Board and Piece.Color 
    (initial state and attacker).
    '''

    self.dropGame()

    if len(args) == 1:
      (board, attacker) = loadBoard(args[0])
    else:
      (board, attacker) = args

    self._game = apnsmod.Game(board, attacker)
    self._posCount = 1

  def loadGame(self, gamePath):
    '''Load an old game from a search file.'''

    self.dropGame()

    self._cancel = False
    (self._game, self._posCount) = apnsmod.loadGame(
      gamePath, Controller._OpCtrl(self, self.loadGameCallbacks)
    )

  def saveGame(self, gamePath):
    '''Save this game into a search file.'''

    self._cancel = False
    if self._game:
      apnsmod.saveGame(self._game, gamePath,
                       Controller._OpCtrl(self, self.saveGameCallbacks))
    else:
      raise RuntimeError('Create or load a game first')

  def resetGame(self):
    '''Drop the current game and load it again.'''

    if self._game:
      initialPos = self._game.initialState
      attacker = self._game.attacker
      self.dropGame()
      self.newGame(initialPos, attacker)
      del initialPos, attacker  # Drop the references to the *old* game.

  def dropGame(self):
    '''Drop the current game and its associated search.'''

    self._search = None
    self._game = None
    self._searchStart = None
    self.stats = None
    self._posCount = None
    self._transTbl = None
    self._proofTbl = None
    self._historyTbl = None
    self._killerDb = None

  def runSearch(self, burst=_MS_BURST_TIME):
    '''Run the search until one of the terminating conditions is met.'''

    self._cancel = False
    self._switchState(Controller.State.INACTIVE)

    if self._game is None:
      raise RuntimeError('Create or load a game first')

    if self._search is None or type(self._search) != self._algoType(self.searchParameters.algo):
      self._search = self._algoType(self.searchParameters.algo)(self._game)

    def numElementsFromMbSize(mbSize, tableType):
      bSize = mbSize * MB
      return bSize / (tableType.sizeOfElement)

    if self.searchParameters.transTblSize is not None:
      ttElems = numElementsFromMbSize(self.searchParameters.transTblSize, apnsmod.TranspositionTable)
    else:
      ttElems = 0

    if self.searchParameters.proofTblSize is not None:
      ptElems = numElementsFromMbSize(self.searchParameters.proofTblSize, apnsmod.ProofTable)
    else:
      ptElems = 0

    if ttElems > 0 and (self._transTbl is None or self._transTbl.size != ttElems):
      self._switchState(Controller.State.ALLOCATING)
      try:
        self._transTbl = apnsmod.TranspositionTable(ttElems)
      except OverflowError:
        raise ValueError('Transposition table size is too large')

    elif ttElems == 0:
      self._transTbl = None

    if ptElems > 0 and (self._proofTbl is None or self._proofTbl.size != ptElems):
      self._switchState(Controller.State.ALLOCATING)
      try:
        self._proofTbl = apnsmod.ProofTable(ptElems)
      except OverflowError:
        raise ValueError('Proof table size is too large')
    elif ptElems == 0:
      self._proofTbl = None

    kCount = int(self.searchParameters.killerCount)
    if kCount > 0 and (self._killerDb is None or self._killerDb.plysSize != kCount):
      self._killerDb = apnsmod.KillerDB(kCount)
    elif kCount == 0:
      self._killerDb = None

    self._search.gcLow = int(self.searchParameters.gcLow)
    self._search.gcHigh = int(self.searchParameters.gcHigh)
    self._search.heurEval = self.searchParameters.heurEval
    self._search.dynWidening = self.searchParameters.dynWidening
    self._search.dynWideningParam = self.searchParameters.dynWideningPar
    self._search.transpositionTable = self._transTbl
    self._search.proofTable = self._proofTbl
    self._search.killerDB = self._killerDb

    logFilename = self.searchParameters.logFilename
    if logFilename is not None:
      if logFilename != '':
        self._search.logSink = apnsmod.FileSink(str(logFilename))
      else:
        self._search.logSink = apnsmod.StdoutSink()
    else:
      self._search.logSink = apnsmod.NullSink()

    self._searchStart = time.clock()
    self._lastMeasurement = time.clock()
    self._lastPosCount = self._posCount
    self._lastPosPerSec = 0

    try:
      self._switchState(Controller.State.SEARCHING)
      while not self._search.finished and not self._limitsExceeded() and not self._cancel:
        self._search.run(burst)
        self._updateProgress()

    except MemoryError:
      self.dropGame()
      raise

    finally:
      self._switchState(Controller.State.INACTIVE)
      self._search.logSink.flush()
      self._posCount = self._search.positionCount
      self.stats = self._makeStats()

  def cancel(self):
    '''Cancel whatever operation is running now.'''
    self._cancel = True

  def getPositionCount(self):
    if self._posCount:
      return self._posCount
    else:
      raise RuntimeError('Create or load a game first')
  positionCount = property(getPositionCount)

  def _updateProgress(self):
    progress = self._makeStats()
    self.searchProgressCallbacks.call(self, progress)

  def _limitsExceeded(self):
    timeExceeded = self.searchParameters.timeLimit and \
        time.clock() - self._searchStart >= self.searchParameters.timeLimit
    posExceeded = self.searchParameters.positionLimit and \
        self._search.positionCount >= self.searchParameters.positionLimit
    memExceeded = self.searchParameters.memoryLimit and \
        self._game.root.subtreeBytes / float(MB) >= \
          self.searchParameters.memoryLimit
    return timeExceeded or memExceeded or posExceeded

  def _makeStats(self):
    now = time.clock()
    progress = SearchProgress()
    progress.timeElapsed = now - self._searchStart
    if self.searchParameters.timeLimit:
      progress.timeLeft = self.searchParameters.timeLimit - progress.timeElapsed
    progress.memUsed = self._game.root.subtreeBytes
    progress.rootPN = self._game.root.proofNumber
    progress.rootDN = self._game.root.disproofNumber
    self._posCount = progress.positionCount = self._search.positionCount
    if now - self._lastMeasurement >= 1.0:
      self._lastPosPerSec = progress.positionsPerSecond = \
          (self._posCount - self._lastPosCount) / (now - self._lastMeasurement)
      self._lastMeasurement = now
      self._lastPosCount = self._posCount
    else:
      progress.positionsPerSecond = self._lastPosPerSec

    tt = self._search.transpositionTable
    if tt:
      progress.transTblSize = tt.memoryUsage
      progress.transTblHits = tt.hits
      progress.transTblMisses = tt.misses

    pt = self._search.proofTable
    if pt:
      progress.proofTblSize = pt.memoryUsage
      progress.proofTblHits = pt.hits
      progress.proofTblMisses = pt.misses

    kDB = self._search.killerDB
    if kDB:
      progress.killerCount  = kDB.totalSize
    else:
      progress.killerCount = 0

    return progress
  
  def _switchState(self, newState):
    changed = newState != self._state
    self._state = newState
    if changed:
      self.stateCallbacks.call(self, newState)

  def _algoType(self, algo):
    return apnsmod.algos[algo][0]
