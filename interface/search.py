from apnsmod import PnSearchAlgo_WinStrategy, WinStrategy

def makeSearch(board, player, strategy):
  '''Make an instance of a PnSearchAlgo using the specified strategy, board and player.

  This is just a wrapper to work-around the fact that pn_search_algo is a template. This function creates an instance of the
  apropriate template instantiation based on the types it is passed.
  '''

  if type(strategy) == WinStrategy:
    return PnSearchAlgo_WinStrategy(board, player, strategy)
  else:
    raise NotImplemented('No matching search algorithm for strategy %s' % type(strategy))

