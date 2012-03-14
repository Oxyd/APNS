'''fileio.py -- file input/output operations'''

import re
import sys
import os
from apnsmod import Board, Piece, Position, Vertex, Step #, dumpTree, loadTree

def _pieceFromLetter(letter):
  '''Given a letter representing a piece, return the piece. The conversion between letters and pieces is done according
  to the official Arimaa notation rules.
  '''

  assert(len(letter) == 1)

  try:
    type = { 'e': Piece.Type.elephant,
             'm': Piece.Type.camel,
             'h': Piece.Type.horse,
             'd': Piece.Type.dog,
             'c': Piece.Type.cat,
             'r': Piece.Type.rabbit }[letter.lower()]

    color = None
    if letter.islower():
      color = Piece.Color.silver
    else:
      color = Piece.Color.gold

    return Piece(color, type)

  except IndexError:
    raise RuntimeError('Bad input file format: %c is not a valid piece character' % letter)


def _letterFromPiece(piece):
  '''Given a piece, return the letter representing it.'''

  letter = { Piece.Type.elephant: 'e',
             Piece.Type.camel: 'm',
             Piece.Type.horse: 'h',
             Piece.Type.dog: 'd',
             Piece.Type.cat: 'c',
             Piece.Type.rabbit: 'r' }[piece.type]

  if piece.color == Piece.Color.gold:
    return letter.upper()
  else:
    return letter


def loadBoard(filename):
  '''loadBoard(filename) -> (Board, Color)

  Load board position from file described by filename. Return the new board and color of the player to take the next turn.

  The expected file format is the format described at http://arimaa.com/arimaa/learn/notation.html .
  '''

  lines = filter(lambda line: len(line) > 0, file(filename, 'r').readlines())  # Only take nonempty lines.

  if len(lines) >= 12:
    # First line needs to indicate turn number and player.
    match = re.match(r'\d([gs])', lines[0])
    if match is None:
      raise RuntimeError('Bad input file format: Expected turn and player on first line')

    player = { 'g': Piece.Color.gold,
               's': Piece.Color.silver }[match.group(1)]

    # Last nonempty line indicates the labelling of the columns.
    lastLine = lines[len(lines) - 1]
    columns = dict()

    for charIndex in xrange(0, len(lastLine)):
      if lastLine[charIndex] in 'abcdefgh':
        if lastLine[charIndex] not in columns:
          columns[lastLine[charIndex]] = charIndex
        else:
          raise RuntimeError('Bad input file format: Invalid column labelling')

      elif lastLine[charIndex] in (' ', '\n'):
        pass  # Space and newline are okay, just skip them.
      else:
        raise RuntimeError('Bad input file format: Invalid column specification')

    if len(columns) != 8:
      raise RuntimeError('Bad input file format: Expected exactly eight columns')

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

    board = Board()
    for row in xrange(1, 9):
      for column in 'abcdefgh':
        pieceLetter = rows[row][columns[column]]

        if pieceLetter not in ('x', 'X', ' '):
          piece = _pieceFromLetter(pieceLetter)
          board.put(Position(row, column), piece)

    return (board, player)

  else:
    raise RuntimeError('Bad input file format: too few rows')

def saveBoard(board, moveNumber, player, filename):
  '''saveBoard(Board, int, Color, str) -> None

  Save the given board to a file with given name. The 'player' parameter specifies the player to move in next turn;
  the 'stepNumber' parameter specifies which move it is.

  The output format is the one described at http://arimaa.com/arimaa/learn/notation.html .

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

    output[outputLine][outputColumn] = _letterFromPiece(piece)

  if filename is not None:
    out = file(filename, 'w')
  else:
    out = sys.stdout

  for line in output:
    out.write(str(line) + '\n')

  if filename is not None:
    out.close()


_VERTICES_EDGES_SEPARATOR = '---'  # Easily recognisable string separating list of vertices from list of edges in the save file.


def _dumpVertices(output, root):
  '''Dump the vertices of the three rooted at 'root' into output. This function will assign each vertex of the tree a unique
  positive integer and set its value as vertex.pickleNumber. The root of the tree is guaranteed to be assigned pickleNumber
  of 1.
  '''

  stack = [ root ]
  number = 1
  while len(stack) > 0:
    vertex = stack.pop()
    if vertex.pickleNumber == 0:
      # This vertex hasn't yet been pickled.
      vertex.pickleNumber = number
      number += 1

      if vertex.leadingStep is not None:  step = vertex.leadingStep.toString()
      else:                               step = 'root'

      if vertex.type_ == Vertex.Type.and_:  type = 'and'
      else:                                 type = 'or'


      output.write('{0}: {1}: {2} {3} {4} {5}\n'.format(vertex.pickleNumber, step, type,
                                                        vertex.stepsRemaining, vertex.proofNumber,
                                                        vertex.disproofNumber))

      for child in vertex.children:
        stack.append(child)


def _dumpEdges(output, root):
  '''Dump the edges of the tree rooted at 'root' into output.'''

  stack = [ root ]

  while len(stack) > 0:
    vertex = stack.pop()

    output.write('{0} : '.format(vertex.pickleNumber))

    for child in vertex.children:
      output.write('{0} '.format(child.pickleNumber))
      stack.append(child)

    output.write('\n')


def _dumpTree(output, root):
  '''Dump the tree rooted at 'root' into output.'''

  _dumpVertices(output, root)
  output.write(_VERTICES_EDGES_SEPARATOR + '\n')
  _dumpEdges(output, root)


def _loadVertices(input):
  '''_loadVertices(input) -> [Vertex]

  Load tree vertices from the given list. Returns a list l where l[index] is the vertex that was originally saved with
  pickleNumber == index - 1. The returned vertices have pickleNumber set to 0.

  The separator string is consumed from the input.
  '''

  vertices = []

  pickleNo = 1
  vertexFormat = re.compile(r'^(\d+): ([a-zA-Z1-8 ]+): (and|or) (\d+) (\d+) (\d+)$')

  for line in input:
    if line.rstrip() != _VERTICES_EDGES_SEPARATOR:
      match = vertexFormat.match(line)
      if match is not None and int(match.group(1)) == pickleNo:
        vertex = Vertex.create()

        stepStr = match.group(2)
        if stepStr == 'root':
          step = None

          if pickleNo != 1:
            raise RuntimeError('Could not load tree from file: root vertex found on a position other than the first')
        else:
          step = Step.fromString(stepStr)
          if step is None:
            raise RuntimeError('Could not load tree from file: invalid step description')

        vertex.leadingStep = step

        if match.group(3) == 'and': vertex.type_ = Vertex.Type.and_
        else:                       vertex.type_ = Vertex.Type.or_

        vertex.stepsRemaining = int(match.group(4))
        vertex.proofNumber = int(match.group(5))
        vertex.disproofNumber = int(match.group(6))

        vertices.append(vertex)
        pickleNo += 1

      else:
        raise RuntimeError('Could not load tree from file: invalid data on input')
    else:
      break

  return vertices


def _loadEdges(input, vertices):
  '''Load edges from input and store them in vertices. vertices is expected to be a list as returned by _loadVertices.'''

  edgeFormat = re.compile(r'^(\d+) : ((?:\d+ )*)$')

  for line in input:
    match = edgeFormat.match(line)
    if match is not None:
      vertexNo = int(match.group(1))
      vertex = vertices[vertexNo - 1]

      if match.group(2) != '':
        children = ( vertices[int(num) - 1] for num in match.group(2).strip().split(' ') )
        for child in children:
          vertex.addChild(child)


def _loadTree(input):
  '''Load the search tree from input and return the root.'''

  vertices = _loadVertices(input)
  _loadEdges(input, vertices)

  if len(vertices) > 0:
    return vertices[0]
  else:
    return None


def _dumpBoardCompact(output, board):
  '''Dump a Board to the given file in a compact, single-line format.'''

  output.write(' '.join(
      ( '{0}{1}{2}'.format(position.row, position.column, _letterFromPiece(piece)) for (position, piece) in board.pieces )
  ))
  output.write('\n')


def _loadBoardCompact(input):
  '''Load a Board from the given file expecting the compact format used by _dumpBoardCompact.'''

  board = Board()
  for element in input.readline().rstrip().split(' '):
    if len(element) != 3:
      raise RuntimeError('Could not load board')

    try:
      row = int(element[0])
    except ValueError:
      raise RuntimeError('Could not load board: invalid row')

    column = element[1]
    piece = _pieceFromLetter(element[2])

    board.put(Position(row, column), piece)

  return board


def saveSearch(search, filename, operationController):
  '''Save the specified search to the given file-like object.'''

  with open(filename, 'w') as output:
    _dumpBoardCompact(output, search.initialBoard)

    if search.player == Piece.Color.gold: player = 'gold'
    else:                                 player = 'silver'
    output.write('{0} {1}\n'.format(player, search.positionCount))

  dumpTree(filename, search.root, True, operationController, search.positionCount)

  if operationController.stop:
    # The operation was cancelled. The file is only partially written. Delete it.
    os.remove(filename)

def loadSearch(filename, operationController):
  '''Load search from given input file.'''

  with open(filename, 'r') as input:
    board = _loadBoardCompact(input)

    searchDescr = input.readline().split(' ')
    if len(searchDescr) != 2:
      raise RuntimeError('Could not load search from file: invalid file format')

    if searchDescr[0] == 'gold':      player = Piece.Color.gold
    elif searchDescr[0] == 'silver':  player = Piece.Color.silver
    else:
      raise RuntimeError('Could not load search from file: invalid player specification')

    try:
      positionCount = int(searchDescr[1])
    except ValueError:
      raise RuntimeError('Could not load search from file: invalid position count')

  tree = loadTree(filename, 2, operationController, positionCount)
  if tree is not None:
    import apnsmod
    search = apnsmod.PNS(board, tree, player, positionCount)
    return search
  else:
    return None
