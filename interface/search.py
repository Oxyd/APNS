from apnsmod import ProofNumberSearch, Game

def makeSearch(board, attacker):
  '''Make an instance of a PnSearchAlgo using the specified strategy, board and attacker.
  '''

  return ProofNumberSearch(Game(board, attacker))

