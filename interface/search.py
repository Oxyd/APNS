from apnsmod import ProofNumberSearch, Game

def makeSearch(board, attacker):
  '''Make an instance of a ProofNumberSearch using the specified board and attacker.'''

  return ProofNumberSearch(Game(board, attacker))
