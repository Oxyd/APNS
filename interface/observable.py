class Observable(object):
  '''Implements the observable pattern. To be used as a base class for Python classes.'''
  
  def __init__(self):
    object.__init__(self)
    self._observers = list()
  

  def addObserver(self, observer):
    '''Add an observer watching for updates of this class.'''
    self._observers.append(observer)
  
  
  def notifyObservers(self, **kwargs):
    '''Call the update() method of all observers, optionally passing any number of arguments.'''
    for observer in self._observers:
      observer.update(self, **kwargs)