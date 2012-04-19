from _apnsmod import SearchAlgorithm, selectBest

class PyPNS(SearchAlgorithm):
  '''A demonstration that we can define our own algorithms in Python too.'''
  
  def __init__(self, game, positionCount=1):
    SearchAlgorithm.__init__(self, game, positionCount)
  
  def doIterate(self):
    self.tree.selectRoot()
    
    while not self.tree.current.leaf:
      selectBest(self.tree)
    
    self.tree.expand()
    self.tree.evaluateChildren()
    self.tree.updatePath()