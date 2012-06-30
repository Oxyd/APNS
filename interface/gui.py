# -*- Encoding: utf-8 -*-

import apnsmod
from interface.observable import Observable
from interface.controller import Controller, SearchParameters, loadBoard, \
    saveBoard

import time, gc, sys, itertools

try:
  import Tkinter
  import tkFileDialog
  import tkMessageBox
  import ttk
except ImportError:
  print >> sys.stderr, 'Importing one of Tkinter modules has failed. ' +\
                       'Please make sure you have Tkinter installed.'
  raise SystemExit(1)

_COLORS = (apnsmod.Piece.Color.gold, apnsmod.Piece.Color.silver)
_TYPES = (apnsmod.Piece.Type.elephant, apnsmod.Piece.Type.camel,
          apnsmod.Piece.Type.horse, apnsmod.Piece.Type.dog,
          apnsmod.Piece.Type.cat, apnsmod.Piece.Type.rabbit)

_BOARD_SIZE = (_BOARD_WIDTH, _BOARD_HEIGHT) = (402, 402)
_TILE_SIZE = (_TILE_WIDTH, _TILE_HEIGHT) = (48, 48)

# Amount of space between the physical board background image border and the
# actual squares on it.
_OUTER_BORDER = 6

# Amount of space between individual squares on the board background image.
_INNER_BORDER = 1

class MainWindow(Observable):
  '''Main window of the application. Displays a toolbar, the search tree and a
  position display. This object will also create an ImageManager instance.
  '''

  imageManager = property(lambda self: self._imageManager)
  window = property(lambda self: self._window)
  searchResultsController = property(lambda self: self._searchResultsCtrl)

  class Command:
    '''Enumeration type that specifies what kind of action the user has
    selected.
    '''
    newSearch, iterate, runSearch, resetSearch, savePosition, loadSearch, \
      saveSearch, close, searchStats = range(9)


  def __init__(self):
    Observable.__init__(self)

    self._window = Tkinter.Tk()
    self._window.title('APNS')
    
    self._imageManager = ImageManager()

    self._toolbar = ttk.Frame(self._window, padding=5)
    self._newSearchBtn = ttk.Button(
      self._toolbar, text='New Initial Position',
      command=lambda: self.notifyObservers(
        command=MainWindow.Command.newSearch))

    self._runBtn = ttk.Button(
      self._toolbar, text='Run Search',
      command=lambda: self.notifyObservers(
        command=MainWindow.Command.runSearch))

    self._resetBtn = ttk.Button(
      self._toolbar, text='Reset Search',
      command=lambda: self.notifyObservers(
        command=MainWindow.Command.resetSearch))

    self._loadSearchBtn = ttk.Button(
      self._toolbar, text='Load Search',
      command=lambda: self.notifyObservers(
        command=MainWindow.Command.loadSearch))

    self._saveSearchBtn = ttk.Button(
      self._toolbar, text='Save Search',
      command=lambda: self.notifyObservers(
        command=MainWindow.Command.saveSearch))

    self._saveBtn = ttk.Button(
      self._toolbar, text='Save Position',
      command=lambda: self.notifyObservers(
        command=MainWindow.Command.savePosition))

    self._statsBtn = ttk.Button(
      self._toolbar, text='Search Stats',
      command=lambda: self.notifyObservers(
        command=MainWindow.Command.searchStats))

    self._window.protocol('WM_DELETE_WINDOW',
        lambda: self.notifyObservers(command=MainWindow.Command.close))

    self.disableSearch()
    self.disableStats()

    self._resultsDisplay = ResultsDisplay(self._window, self._imageManager)
    self._searchResultsCtrl = ResultsController(self._resultsDisplay)

    self._newSearchBtn.grid(row=0, column=0, padx=(0, 3))
    self._runBtn.grid(row=0, column=1, padx=(3, 3))
    self._resetBtn.grid(row=0, column=2, padx=(3, 3))
    self._loadSearchBtn.grid(row=0, column=3, padx=(3, 3))
    self._saveSearchBtn.grid(row=0, column=4, padx=(3, 3))
    self._saveBtn.grid(row=0, column=5, padx=(3, 3))
    self._statsBtn.grid(row=0, column=6, padx=(3, 0))

    self._toolbar.grid(row=0, column=0, sticky='W')
    self._resultsDisplay.widget.grid(row=1, column=0, sticky='NSEW')

    self._window.columnconfigure(0, weight=1)
    self._window.rowconfigure(1, weight=1)


  def mainloop(self):
    '''Start the Tk main loop.'''
    self._window.mainloop()


  def enableSearch(self):
    '''Enable the Run Search and Iterate buttons.'''

    self._runBtn['state'] = ['!disabled']
    self._resetBtn['state'] = ['!disabled']
    self._saveSearchBtn['state'] = ['!disabled']
    self._saveBtn['state'] = ['!disabled']


  def disableSearch(self):
    '''Disable the Run Search and Iterate buttons.'''

    self._runBtn['state'] = ['disabled']
    self._resetBtn['state'] = ['disabled']
    self._saveSearchBtn['state'] = ['disabled']
    self._saveBtn['state'] = ['disabled']


  def enableStats(self):
    self._statsBtn['state'] = ['!disabled']


  def disableStats(self):
    self._statsBtn['state'] = ['disabled']


  def destroy(self):
    '''Destroy this window.'''

    self._window.quit()


class MainWindowController(object):
  '''Control logic of the main window. This class creates the actual search 
  object.
  '''

  def __init__(self, mainWindowDsply):
    object.__init__(self)

    self._mainWindowDsply = mainWindowDsply
    self._mainWindowDsply.addObserver(self)
    self._resultsCtrl = mainWindowDsply.searchResultsController
    self._controller = Controller()
    self._searchPreferences = SearchParameters()

  def runApplication(self):
    '''Start the GUI of the application.'''

    try:
      self._mainWindowDsply.mainloop()
    except MemoryError:
      del self._search
      gc.collect()
      tkMessageBox.showerror('Fatal Error', 'Ran out of memory. Aborting.')


  def update(self, source, command):
    '''React to user's commands.'''

    if command == MainWindow.Command.newSearch:
      positionEditorDlg = PositionEditorDialog(
        self._mainWindowDsply.window,
        'Create or load initial position',
        'Specify an initial search position or load one from disk:',
        self._mainWindowDsply.imageManager
      )

      if positionEditorDlg.board is not None:
        self._controller.newGame(positionEditorDlg.board,
                                 positionEditorDlg.player)
        self._resultsCtrl.updateTree(self._controller)
        gc.collect()
        self._mainWindowDsply.disableStats()
        self._mainWindowDsply.enableSearch()

    elif command == MainWindow.Command.runSearch:
      algoList = []
      for algo in apnsmod.algos:
        algoList.append((algo, apnsmod.algos[algo][1]))
      
      dlg = RunSearchDialog(self._mainWindowDsply.window, algoList)
      runSearchCtrl = RunSearchController(dlg, self._searchPreferences)
      (doRun, newPrefs) = runSearchCtrl.run()

      if doRun:
        if newPrefs is not None:
          self._searchPreferences = newPrefs

        pars = self._controller.searchParameters
        pars.algo = newPrefs.algo
        pars.timeLimit = newPrefs.timeLimit if newPrefs.timeLimitCheck \
                                            else None
        pars.positionLimit = \
            newPrefs.positionLimit if newPrefs.positionLimitCheck else None
        pars.memoryLimit = \
            newPrefs.memLimit if newPrefs.memLimitCheck else None
        pars.transTblSize = newPrefs.transTblSize
        pars.proofTblSize = newPrefs.proofTblSize
        pars.killerCount = newPrefs.killerCount
        pars.logFilename = newPrefs.logPath if newPrefs.logCheck else None
        
        if newPrefs.gcCheck:
          pars.gcHigh = newPrefs.gcHigh
          pars.gcLow = newPrefs.gcLow
        else:
          pars.gcHigh = pars.gcLow = 0
        
        #pars.moveCacheSize = newPrefs.moveCacheSize

        try:
          dlg = SearchProgressDialog(self._mainWindowDsply.window,
                                     runSearchCtrl.showTimeLeft, running=True)
          searchProgressCtrl = SearchProgressController(
            dlg, runSearchCtrl.timeLimit, runSearchCtrl.posLimit,
            runSearchCtrl.memLimit
          )
          self._runStats = searchProgressCtrl.run(self._controller)
          self._mainWindowDsply.enableStats()

        except MemoryError:
          tkMessageBox.showerror('Out of memory',
            'This program ran out of memory while trying to expand the tree.\n'
            +'The partial tree has been discarded.'
          )
          self._mainWindowDsply.disableSearch()
          self._mainWindowDsply.disableStats()

        self._resultsCtrl.updateTree(self._controller)
        gc.collect()
        self._mainWindowDsply.window.focus_set()

    elif command == MainWindow.Command.resetSearch:
      if tkMessageBox.askyesno('Reset', 'Discard whole search?'):
        self._controller.resetGame()
        self._resultsCtrl.updateTree(self._controller)
        self._mainWindowDsply.disableStats()
        gc.collect()

    elif command == MainWindow.Command.savePosition:
      filename = tkFileDialog.asksaveasfilename()
      if len(filename) > 0:
        self._resultsCtrl.savePosition(str(filename), self._controller)

    elif command == MainWindow.Command.loadSearch:
      filename = tkFileDialog.askopenfilename()
      if len(filename) > 0:
        self._controller.dropGame()
        gc.collect()

        dlg = SaveLoadProgressDialog(
          self._mainWindowDsply.window,
          'Loading search tree',
          'Loading the search tree.\nThis might take a while'
        )

        LoadProgressController(self._controller, dlg, str(filename))
        self._resultsCtrl.updateTree(self._controller)

        if self._controller.gameLoaded:
          self._mainWindowDsply.enableSearch()
          self._mainWindowDsply.disableStats()

    elif command == MainWindow.Command.saveSearch:
      filename = tkFileDialog.asksaveasfilename()
      if len(filename) > 0:
        dlg = SaveLoadProgressDialog(
          self._mainWindowDsply.window,
          'Saving search tree',
          'Saving the search tree.\nThis might take a while'
        )
        SaveProgressController(self._controller, dlg, str(filename))

    elif command == MainWindow.Command.searchStats:
      dlg = SearchProgressDialog(
        self._mainWindowDsply.window, showTimeLeft=False, running=False
      )
      SearchStatsController(dlg, self._controller.stats)

    elif command == MainWindow.Command.close:
      self._mainWindowDsply.destroy()


class ResultsDisplay(Observable):
  '''A widget that displays the search results. It displays the search tree and
  for each selected node in the tree, it displays the corresponding board
  position. '''

  widget = property(lambda self: self._content)
  boardController = property(lambda self: self._boardCtrl)

  class Command:
    show, expand = range(2)

  def __init__(self, parent, imageManager):
    Observable.__init__(self)

    self._content = ttk.Frame(parent, padding=5)

    self._tree = ttk.Treeview(self._content)
    self._tree['columns'] = ('best', 'type', 'pn', 'dn')
    self._tree['selectmode'] = 'browse'
    self._tree.column('#0', stretch=True, width=300, minwidth=300)
    self._tree.column(0, stretch=False, width=20, minwidth=20)
    self._tree.column(1, stretch=False, width=60, minwidth=60)
    self._tree.column(2, stretch=False, width=60, minwidth=60)
    self._tree.column(3, stretch=False, width=60, minwidth=60)
    self._tree.heading('#0', text='Step')
    self._tree.heading(0, text='**')
    self._tree.heading(1, text='Type')
    self._tree.heading(2, text='PN')
    self._tree.heading(3, text='DN')

    verticalScrollBar = ttk.Scrollbar(
        self._content, orient=Tkinter.VERTICAL, command=self._tree.yview)
    horizontalScrollBar = ttk.Scrollbar(
        self._content, orient=Tkinter.HORIZONTAL, command=self._tree.xview)
    self._tree['yscrollcommand'] = verticalScrollBar.set
    self._tree['xscrollcommand'] = horizontalScrollBar.set

    self._tree.bind('<<TreeviewSelect>>', self._select)
    self._tree.bind('<<TreeviewOpen>>', self._open)

    self._position = BoardDisplay(self._content, imageManager)
    self._boardCtrl = BoardController(None, self._position, None)

    self._tree.grid(row=0, column=0, sticky='NSEW')
    verticalScrollBar.grid(row=0, column=1, sticky='NS', padx=(3, 7))
    horizontalScrollBar.grid(row=1, column=0, sticky='WE', pady=(3, 7))
    self._position.widget.grid(row=0, column=2, sticky='N')

    self._content.rowconfigure(0, weight=1)
    self._content.columnconfigure(0, weight=1)


  def addNode(self, parent, name, type_, pn, dn, best=False, principal=False):
    '''Add a node to the tree. Return a handle of the node, which can later be
    used to remove the node, or attach a child to it.

    parent is the handle of the parent node, or None if it is a top-level node.
    '''

    if parent is None:
      parent = ''

    if principal:
      mark = '**'
    elif best:
      mark = '*'
    else:
      mark = ''

    handle = self._tree.insert(parent, 'end', text=name,
                               values=(mark, type_, pn, dn))
    return handle


  def removeNode(self, handle):
    '''Remove a node from the tree. handle is the value returned from the
    corresponding addNode call.
    '''

    self._tree.delete(handle)


  def updateNode(self, handle, type_, pn, dn, best=False, principal=False):
    '''Change the PN and DN values of a node.'''

    if principal:
      mark = '**'
    elif best:
      mark = '*'
    else:
      mark = ''

    self._tree.item(handle, values=(mark, type_, pn, dn))


  def selectNode(self, handle):
    '''Set the selected handle to the specified one.'''

    self._tree.selection('set', handle)


  def showBoard(self, newBoard):
    '''Show new content in the Board display.'''

    self._boardCtrl.attachToBoard(newBoard)


  def selected(self):
    '''Return the handle of the selected node.'''

    (sel,) = self._tree.selection()
    return sel


  def getNodeStep(self, node):
    '''Get the step string of a given node.'''

    return self._tree.item(node, 'text')


  def getNodeType(self, node):
    '''Return the type of a given node.'''

    return self._tree.set(node, 'type')


  def isRoot(self, node):
    '''Decide whether given node is the root node.'''

    return node == ''


  def getParent(self, node):
    '''Return the handle of the parent node.'''

    return self._tree.parent(node)


  def _select(self, e):
    '''Handle change of selection in the tree.'''
    handle = self._tree.selection()[0]
    self.notifyObservers(handle=handle, command=ResultsDisplay.Command.show)


  def _open(self, e):
    '''Handle the expansion of a vertex in the tree.'''
    handle = self._tree.selection()[0]
    self.notifyObservers(handle=handle, command=ResultsDisplay.Command.expand)


def strFromNum(n):
  '''Return the string representation of n as a proof-/disproof-number.'''

  if n < apnsmod.Vertex.infty:
    return unicode(n)
  else:
    return u'∞'


class ResultsController(object):
  '''Controller of ResultsDisplay.'''

  # XXX: This thing *badly* needs refactoring. Seriously.

  class _DisplayNode(object):
    '''A node in the display. This represents the tree that is actually
    displayed. It contains a list of its _DisplayNode children, its
    _DisplayNode parent and a reference to the actual node in the real search
    tree.
    '''

    _handleToNode = {}  # A dictionary handle -> _DisplayNode.

    @staticmethod
    def release():
      '''Release references to vertices in the tree.'''

      ResultsController._DisplayNode._handleToNode = {}


    def __init__(self, parent, vertex, display, best, principal):
      '''Create a node and attach it to the display.'''

      object.__init__(self)

      self.vertex = vertex  # The vertex in the search tree
      self.children = []    # _DisplayNode children of this node.
      self.parent = parent  # _DisplayNode parent of this node.
      self._best = best
      self.principal = principal

      if self.vertex.step is not None:
        name = self.vertex.step.toString()
      elif parent is not None:
        name = 'lambda'
      else:
        name = 'root'
      
      parent = self.parent.handle if self.parent is not None else None
      self.handle = display.addNode(parent, name, self._getType(),
                                    strFromNum(self.vertex.proofNumber),
                                    strFromNum(self.vertex.disproofNumber),
                                    best, principal)
      ResultsController._DisplayNode._handleToNode[self.handle] = self


    best = property(lambda self: self._best)


    @staticmethod
    def getNode(handle):
      '''Given a handle, return the apropriate node from the tree.'''

      return ResultsController._DisplayNode._handleToNode[handle]


    def updateDisplay(self, display):
      '''Update the values in the display.'''

      display.updateNode(self.handle, self._getType(),
                         strFromNum(self.vertex.proofNumber),
                         strFromNum(self.vertex.disproofNumber),
                         self.best, self.principal)


    def setBest(self, newBest, display):
      '''Toggle the 'best' flag of this vertex.'''

      self._best = newBest
      self.updateDisplay(display)


    def setPrincipal(self, newPrincipal, display):
      '''Toggle the 'principal' flag of this vertex.'''

      if newPrincipal != self.principal:
        self.principal = newPrincipal

        if self.principal:
          for child in self.children:
            if child.best: child.setPrincipal(True, display)
        else:
          for child in self.children:
            child.setPrincipal(False, display)

        self.updateDisplay(display)


    def expand(self, display):
      '''Add children of this vertex to the display.'''

      assert len(self.children) == 0

      sameTypeChildren  = (
        c for c in self.vertex.children if c.type_ == self.vertex.type_
      )
      otherTypeChildren = (
        c for c in self.vertex.children if c.type_ != self.vertex.type_
      )

      for child in itertools.chain(sameTypeChildren, otherTypeChildren):
        self.children.append(
          ResultsController._DisplayNode(self, child, display, False, False)
        )


    def _getType(self):
      return 'AND' if self.vertex.type_ == apnsmod.Vertex.Type.and_ else 'OR'


  def __init__(self, resultsDisplay):
    object.__init__(self)

    self._resultsDsply = resultsDisplay
    self._resultsDsply.addObserver(self)
    self._tree = None
    self._initialBoard = None


  def savePosition(self, filename, controller):
    '''Save the currently displayed position to the given file.'''

    selectedHandle = self._resultsDsply.selected()
    turn = 1  # Ignore turn numbers.

    if self._resultsDsply.getNodeType(selectedHandle) == 'AND':
      player = apnsmod.opponentColor(controller.attacker)
    else:
      player = controller.attacker

    if player == apnsmod.Piece.Color.gold:
      p = 'g'
    else:
      p = 's'

    saveBoard(self._resultsDsply.boardController.board, turn, p, filename)


  def update(self, source, handle, command):
    '''React to user's commands from the results display. Namely, display board
    for the selected node of the search tree and handle the expansion of a
      vertex.
    '''

    if self._tree is None: return

    if command == ResultsDisplay.Command.show:
      # Display the board corresponding to the newly-selected vertex.

      board = self._getDisplayedBoard(handle)
      self._resultsDsply.showBoard(board)

    elif command == ResultsDisplay.Command.expand:
      self._expandVertex(handle)


  def updateTree(self, controller):
    '''Update the display of the search tree. This will first update the PN and
    DN values of all nodes already displayed, then it will attach new nodes --
    that are not displayed yet -- to the display.
    '''

    if self._tree:
      self._resultsDsply.removeNode(self._tree.handle)
      ResultsController._DisplayNode.release()
      self._tree = None

    if controller.root is not None:
      self._tree = ResultsController._DisplayNode(
        None, controller.root, self._resultsDsply, True, True
      )
      self._initialState = controller.initialState.copy()
      self._tree.expand(self._resultsDsply)
      self._updateBest(self._tree)
      self._resultsDsply.selectNode(self._tree.handle)
    else:
      self._resultsDsply.showBoard(None)


  def _expandVertex(self, handle):
    '''Expand a vertex. This needs a check whether any of the children's
    vertices have any children which are not in the tree yet, and if so add
    them to the tree.
    '''

    self._resultsDsply.selectNode(handle)
    node = ResultsController._DisplayNode.getNode(handle)

    for child in node.children:
      if len(child.children) == 0 and len(list(child.vertex.children)) > 0:
        child.expand(self._resultsDsply)
        self._updateBest(child)


  def _updateBest(self, vertex):
    '''Go through all of vertex's children and mark the best one.'''

    best = apnsmod.bestSuccessor(vertex.vertex)
    for child in vertex.children:
      isBest = hash(child.vertex) == hash(best)
      child.setBest(isBest, self._resultsDsply)
      if vertex.principal:
        child.setPrincipal(isBest, self._resultsDsply)


  def _getDisplayedBoard(self, vertex):
    '''Get the board object corresponding to the selected tree position.'''

    # First, go up the tree, saving all vertices on the path in a stack.
    stack = []
    while not self._resultsDsply.isRoot(vertex):
      stack.append(self._resultsDsply.getNodeStep(vertex))
      vertex = self._resultsDsply.getParent(vertex)

    # Now, having the path on the stack, go back down, transforming the initial
    # board into the final result.
    result = self._initialState.copy()
    while len(stack) > 0:
      step = apnsmod.Step.fromString(stack.pop())
      if step is not None:
        apnsmod.apply(step, result)

    return result


  def _addToDisplay(self, parent, parentHandle, child, bold):
    '''Add a node to the display.

    Parameters:
    -- parent: Parent vertex of the to-be-added vertex
    -- parentHandle: Handle of parent in the tree display
    -- child: Child to be added
    -- bold: If true, the newly added child will be emphasised on the display
    '''

    if child.leadingStep is not None:
      name = child.leadingStep.toString()
    else:
      name = 'Initial Position'

    h = self._resultsDsply.addNode(
      parentHandle,
      name,
      self._nodeType(child),
      strFromNum(child.proofNumber), strFromNum(child.disproofNumber),
      bold
    )
    self._nodeToHandle[hash(child)] = h
    self._handleToNodeParent[h] = (child, parent)


  def _nodeType(self, node):
    '''Return the string representation of a node's type. It's either 'AND' or
    'OR'.
    '''

    if node.type_ == apnsmod.Vertex.Type.and_:
      return 'AND'
    else:
      return 'OR'


class DialogWindow(object):
  '''A generic dialog window.

  It has three properties:
    -- window: an instance of Tkinter.Toplevel -- it is the window widget 
        itself
    -- content: a frame in the window where any meaningful content should be 
        placed
    -- buttonBox: a frame at the bottom of the window, where standard buttons 
        such as 'Ok' or 'Cancel' are to be placed.
  '''

  window = property(lambda self: self._window)
  content = property(lambda self: self._contentFrame)
  buttonBox = property(lambda self: self._buttonsFrame)
  parent = property(lambda self: self._parent)

  def __init__(self, parent, title, deleteAction=None):
    '''Make a new dialog window and display it on screen.'''

    object.__init__(self)

    self._parent = parent

    self._window = Tkinter.Toplevel(parent, takefocus=True)
    self._window.title(title)
    self._window.transient(self._parent)

    if deleteAction is None:
      deleteAction = self.close

    self.setDeleteAction(deleteAction)

    self._frame = ttk.Frame(self._window)
    self._contentFrame = ttk.Frame(self._frame)
    separator = ttk.Separator(self._frame)
    self._buttonsFrame = ttk.Frame(self._frame)

    self._contentFrame.grid(row=0, column=0, sticky='NSWE')
    separator.grid(row=1, column=0, sticky='WE', pady=10)
    self._buttonsFrame.grid(row=2, column=0, sticky='E')

    self._frame.rowconfigure(0, weight=1)
    self._frame.columnconfigure(0, weight=1)
    self._frame.grid(row=0, column=0, sticky='NSEW')
    self._window.rowconfigure(0, weight=1)
    self._window.columnconfigure(0, weight=1)
    self._frame['padding'] = 5


  def run(self, wait=True):
    '''Run the dialog modal loop. If wait is True, this function will return
    only after this dialog has been destroyed, otherwise it returns
    immediately.
    '''

    self._window.update_idletasks()
    self._window.grab_set()
    self._window.focus_set()
    if wait:
      self._window.wait_window(self._window)


  def close(self):
    '''Close this dialog and return control back to the parent.'''

    self._window.grab_release()
    self._window.destroy()
    if self._parent is not None:
      self._parent.focus_set()
  
  
  def setDeleteAction(self, newAction):
    '''Set a new action that will be called when the dialog is closed.'''

    self._window.protocol('WM_DELETE_WINDOW', newAction)



class SaveLoadProgressDialog(Observable):
  '''A simple dialog window showing the progress of the Save Tree or Load Tree
  actions.
  '''

  window = property(lambda self: self._dialog.window)

  def __init__(self, parent, title, text):
    Observable.__init__(self)

    self._dialog = DialogWindow(parent, title)
    self._dialog.window.minsize(width=150, height=1)

    self._lbl = ttk.Label(self._dialog.content, text=text + '   ')

    cancelBtn = ttk.Button(self._dialog.buttonBox, text='Cancel', 
                           command=lambda: self.notifyObservers())
    self._dialog.setDeleteAction(lambda: cancelBtn.invoke())

    self._lbl.grid(row=0, column=0, sticky='W', padx=10, pady=5)
    cancelBtn.grid(row=0, column=0, sticky='E')

    self._dialog.content.rowconfigure(1, weight=1)
    self._dialog.buttonBox.columnconfigure(0, weight=1)

    self._dialog.content.focus_set()
    self._dialog.window.bind('<Escape>', lambda e: cancelBtn.invoke())


  def tick(self):
    '''Make a simple dots animation.'''

    text = self._lbl['text']
    dots = 0
    for i in xrange(len(text) - 3, len(text)):
      if text[i] == '.': dots += 1

    if dots < 3:
      self._lbl['text'] = text[:len(text) - 3 + dots] + '.' + \
                          (3 - dots - 1) * ' '
    else:
      self._lbl['text'] = text[:len(text) - 3] + '   '


  def run(self):
    '''Show the dialog on screen.'''

    self._dialog.run(wait=False)


  def close(self):
    '''Close the dialog window.'''
    self._dialog.close()


class Canceller:
  def __init__(self, controller, dialog):
    self.controller = controller
    self.dialog = dialog
    self.lastTick = time.time()

  def callback(self, controller):
    self.dialog.window.update()

    now = time.time()
    if now - self.lastTick > 0.5:
      self.dialog.tick()
      self.lastTick = now

  def update(self, source):
    '''Update from the dialog -- this means the user wishes to cancel the
    operation.
    '''
    self.controller.cancel()


class SaveProgressController(object):
  def __init__(self, controller, dialog, filename):
    object.__init__(self)

    canceller = Canceller(controller, dialog)
    controller.saveGameCallbacks.add(canceller.callback)
    dialog.addObserver(canceller)
    dialog.run()

    try:
      controller.saveGame(filename)
    except RuntimeError, e:
      tkMessageBox.showerror(
        'Could not savePosition search',
        'Saving search to {0} failed:\n{1}'.format(filename, e)
      )

    controller.saveGameCallbacks.remove(canceller.callback)
    dialog.close()


class LoadProgressController(object):
  search = property(lambda self: self._search)

  def __init__(self, controller, dlg, filename):
    object.__init__(self)

    canceller = Canceller(controller, dlg)
    controller.loadGameCallbacks.add(canceller.callback)
    dlg.addObserver(canceller)
    dlg.run()

    try:
      controller.loadGame(filename)
    except RuntimeError, e:
      tkMessageBox.showerror(
        'Could not load search',
        'Loading the search tree from {0} failed:\n{1}'.format(filename, e)
      )

    controller.loadGameCallbacks.remove(canceller.callback)
    dlg.close()


class RunSearchDialog(Observable):
  '''A dialog window that lets the user choose search parameters and then
  execute a search.'''

  parent = property(lambda self: self._dialog.parent)
  algo = property(lambda self: self._algoVar.get(),
                  lambda self, val: self._setAlgo(val))
  timeLimit = property(lambda self: self._timeLimit.get(),
                       lambda self, val: self.setTimeLimit(val))
  timeLimitCheck = property(lambda self: self._timeLimitCheckVar.get() == '1',
                            lambda self, val: self.enableTimeLimit(val))
  positionLimit = property(lambda self: self._positionLimitVar.get(),
                           lambda self, val: self.setPositionLimit(val))
  positionLimitCheck = property(
    lambda self: self._positionLimitCheckVar.get() == '1',
    lambda self, val: self.enablePositionLimit(val)
  )
  memLimit = property(lambda self: self._memLimitVar.get(),
                      lambda self, val: self.setMemLimit(val))
  memLimitCheck = property(lambda self: self._memLimitCheckVar.get() == '1',
                           lambda self, val: self.enableMemLimit(val))
  transTblSize = property(lambda self: self._memorySpinVar.get(),
                          lambda self, val: self.setTransTblSize(val))
  proofTblSize = property(lambda self: self._proofSpinVar.get(),
                          lambda self, val: self.setProofTblSize(val))
  killerCount = property(lambda self: self._killerCountVar.get(),
                         lambda self, val: self.setKillerCount(val))
  logCheck = property(
    lambda self: self._logCheckVar.get() == '1',
    lambda self, val: self.enableLog(val)
  )
  logPathName = property(
    lambda self: self._logPathNameVar.get(),
    lambda self, val: self.setLogPathName(val)
  )
  #moveCacheSize = property(lambda self: self._moveCacheSpinVar.get(),
  #                         lambda self, val: self.setMoveCache(val))
  gcCheck = property(
    lambda self: self._gcCheckVar.get() == '1',
    lambda self, val: self.enableGC(val)
  )
  gcHigh = property(
    lambda self: self._gcHigh.get(),
    lambda self, val: self.setGCHigh(val)
  )
  gcLow = property(
    lambda self: self._gcLow.get(),
    lambda self, val: self.setGCLow(val)
  )

  class Command:
    timeLimitCheck, positionLimitCheck, memLimitCheck, gcCheck, \
      logCheck, logChoose, start = range(7)

  def __init__(self, parent, algos):
    '''Create the dialog.'''

    Observable.__init__(self)

    self._dialog = DialogWindow(parent, 'Run Search')

    infoLabel = ttk.Label(self._dialog.content, text='Enter parameters of the search:')
    algoFrame = ttk.Labelframe(self._dialog.content, text='Algorithm:', padding=5)
    
    self._algoVar = Tkinter.StringVar(value=algos[0][0])
    algoRadios = []
    for row, (algo, description) in enumerate(algos):
      radio = ttk.Radiobutton(algoFrame, text=description,
                              variable=self._algoVar, value=algo)
      radio.grid(row=row, column=0, columnspan=3, sticky='WE')
      algoRadios.append(radio)
    
    nextRow = len(algos)
    
    self._gcCheckVar = Tkinter.StringVar(value='1')
    gcCheck = ttk.Checkbutton(
      algoFrame, text='Garbage Collector',
      variable=self._gcCheckVar,
      command=lambda: 
        self.notifyObservers(command=RunSearchDialog.Command.gcCheck)
    )
    gcHighLabel = ttk.Label(algoFrame, text='High threshold:')
    gcLowLabel = ttk.Label(algoFrame, text='Low threshold:')
    gcHighUnits = ttk.Label(algoFrame, text='vertices')
    gcLowUnits = ttk.Label(algoFrame, text='vertices')
    
    self._gcHigh = Tkinter.StringVar(value='5000000')
    self._gcLow = Tkinter.StringVar(value='3000000')
    
    self._gcHighSpin = Tkinter.Spinbox(
      algoFrame,
      from_=1, to=999999999999999,
      textvariable=self._gcHigh
    )
    self._gcLowSpin = Tkinter.Spinbox(
      algoFrame,
      from_=1, to=999999999999999,
      textvariable=self._gcLow
    )
    
    gcCheck.grid(row=nextRow, column=0, columnspan=3, sticky='WE', pady=(5, 0))
    
    gcHighLabel.grid(row=nextRow + 1, column=0, sticky='E', padx=(30, 0))
    self._gcHighSpin.grid(row=nextRow + 1, column=1, sticky='WE', padx=5)
    gcHighUnits.grid(row=nextRow + 1, column=2, sticky='W')
    
    gcLowLabel.grid(row=nextRow + 2, column=0, sticky='E', padx=(30, 0))
    self._gcLowSpin.grid(row=nextRow + 2, column=1, sticky='WE', padx=5)
    gcLowUnits.grid(row=nextRow + 2, column=2, sticky='W')
    
    limitsFrame = ttk.Labelframe(self._dialog.content,
                                 text='Search limits:', padding=5)

    self._timeLimitCheckVar = Tkinter.StringVar(value='0')
    self._timeLimitCheck = ttk.Checkbutton(
      limitsFrame, text='Time limit:',
      variable=self._timeLimitCheckVar,
      command=lambda: self.notifyObservers(
        command=RunSearchDialog.Command.timeLimitCheck))

    self._timeLimit = Tkinter.StringVar(value='0')
    self._timeLimitSpin = Tkinter.Spinbox(limitsFrame, from_=1, to=9000,
                                          textvariable=self._timeLimit)
    timeLimitUnits = ttk.Label(limitsFrame, text='seconds')

    self._positionLimitCheckVar = Tkinter.StringVar(value='0')
    self._positionLimitCheck = ttk.Checkbutton(
      limitsFrame, text='Position limit:',
      variable=self._positionLimitCheckVar,
      command=lambda: self.notifyObservers(
        command=RunSearchDialog.Command.positionLimitCheck))
    self._positionLimitVar = Tkinter.StringVar(value='0')
    self._positionLimitSpin = Tkinter.Spinbox(
      limitsFrame, from_=1, to=99999999999,
      textvariable=self._positionLimitVar)
    positionLimitUnits = ttk.Label(limitsFrame, text='positions')

    self._memLimitCheckVar = Tkinter.StringVar(value='0')
    self._memLimitCheck = ttk.Checkbutton(
      limitsFrame, text='Memory limit:',
      variable=self._memLimitCheckVar,
      command=lambda: self.notifyObservers(
        command=RunSearchDialog.Command.memLimitCheck))
    self._memLimitVar = Tkinter.StringVar(value='0')
    self._memLimitSpin = Tkinter.Spinbox(limitsFrame, from_=1, to=999999999999,
                                         textvariable=self._memLimitVar)
    memLimitUnits = ttk.Label(limitsFrame, text='MB')

    algoParamsFrame = ttk.Labelframe(self._dialog.content,
                                     text='Algorithm parameters:', padding=5)
    #moveCacheLabel = ttk.Label(algoParamsFrame, text='Move cache size: ')

    sizeLabel = ttk.Label(algoParamsFrame, text='Transposition table size: ')
    sizeUnits = ttk.Label(algoParamsFrame, text='MB')

    proofLabel = ttk.Label(algoParamsFrame, text='Proof table size: ')
    proofUnits = ttk.Label(algoParamsFrame, text='MB')
    
    killerLabel = ttk.Label(algoParamsFrame, text='Killers for each level: ')

    self._memorySpinVar = Tkinter.StringVar(value='0')
    self._proofSpinVar = Tkinter.StringVar(value='0')
    self._killerCountVar = Tkinter.StringVar(value='0')
    #self._moveCacheSpinVar = Tkinter.StringVar(value='0')

    #self._moveCacheSpin = Tkinter.Spinbox(algoParamsFrame, from_=0, to=1024,
    #                                      textvariable=self._moveCacheSpinVar)
    self._memorySpin = Tkinter.Spinbox(algoParamsFrame, from_=0, to=99999999,
                                       textvariable=self._memorySpinVar)
    self._proofSpin = Tkinter.Spinbox(algoParamsFrame, from_=0, to=99999999,
                                      textvariable=self._proofSpinVar)
    self._killersSpin = Tkinter.Spinbox(algoParamsFrame, from_=0, to=100,
                                        textvariable=self._killerCountVar)
    
    loggingFrame = ttk.Labelframe(self._dialog.content,
                                  text='Logging:', padding=5)
    
    self._logCheckVar = Tkinter.StringVar(value='0')
    self._loggingCheck = ttk.Checkbutton(
      loggingFrame, text='Enabled', variable=self._logCheckVar,
      command=lambda: self.notifyObservers(
        command=RunSearchDialog.Command.logCheck
      )
    )
    
    self._logPathLabel = ttk.Label(loggingFrame, text='Log file:')
    
    self._logPathNameVar = Tkinter.StringVar(value='')
    self._logPathName = ttk.Entry(loggingFrame, 
                                  textvariable=self._logPathNameVar)
    
    self._logPathChooseBtn = ttk.Button(
      loggingFrame, text='Choose…',
      command=lambda: self.notifyObservers(
        command=RunSearchDialog.Command.logChoose
      )
    )
    
    self._loggingCheck.grid(row=0, column=0, columnspan=3, sticky='WE')
    self._logPathLabel.grid(row=1, column=0, sticky='E', padx=(30, 5), pady=3)
    self._logPathName.grid(row=1, column=1, sticky='WE', pady=3)
    self._logPathChooseBtn.grid(row=1, column=2, sticky='W', padx=5, pady=3)

    runBtn = ttk.Button(self._dialog.buttonBox, text='Run',
                        command=self._execute)
    cancelBtn = ttk.Button(self._dialog.buttonBox, text='Cancel',
                           command=self._dialog.close)

    self._timeLimitCheck.grid(row=0, column=0, sticky='W')
    self._timeLimitSpin.grid(row=0, column=1, sticky='WE', padx=5)
    timeLimitUnits.grid(row=0, column=2, sticky='W')

    self._positionLimitCheck.grid(row=1, column=0, sticky='W')
    self._positionLimitSpin.grid(row=1, column=1, sticky='WE', padx=5)
    positionLimitUnits.grid(row=1, column=2, sticky='W')

    self._memLimitCheck.grid(row=2, column=0, sticky='W')
    self._memLimitSpin.grid(row=2, column=1, sticky='WE', padx=5)
    memLimitUnits.grid(row=2, column=2, sticky='W')

    algoFrame.columnconfigure(1, weight=1)
    algoFrame.grid(row=1, column=0, sticky='WE', pady=(0, 5))

    limitsFrame.columnconfigure(1, weight=1)

    infoLabel.grid(row=0, column=0, pady=5, sticky='W')
    limitsFrame.grid(row=2, column=0, sticky='EW', pady=(5, 5))

    #moveCacheLabel.grid(row=1, column=0, sticky='W')
    #self._moveCacheSpin.grid(row=1, column=1, sticky='WE', padx=5)

    sizeLabel.grid(row=1, column=0, sticky='W')
    self._memorySpin.grid(row=1, column=1, sticky='WE', padx=5)
    sizeUnits.grid(row=1, column=2)

    proofLabel.grid(row=2, column=0, sticky='W')
    self._proofSpin.grid(row=2, column=1, sticky='WE', padx=5)
    proofUnits.grid(row=2, column=2)
    
    killerLabel.grid(row=3, column=0, sticky='W')
    self._killersSpin.grid(row=3, column=1, sticky='WE', padx=5)

    algoParamsFrame.columnconfigure(1, weight=1)
    algoParamsFrame.grid(row=4, column=0, sticky='EW', pady=(5, 5))
    
    loggingFrame.columnconfigure(1, weight=1)
    loggingFrame.grid(row=5, column=0, sticky='WE', pady=(5, 0))

    runBtn.grid(row=0, column=0, padx=5)
    cancelBtn.grid(row=0, column=1)

    self._dialog.buttonBox.rowconfigure(0, weight=1)
    self._dialog.content.columnconfigure(0, weight=1)

    self._dialog.window.bind('<Return>', lambda e: self._execute())
    self._dialog.window.bind('<Escape>', lambda e: self.close())

    self._timeLimitSpin.focus_set()


  def run(self):
    '''Enter the modal dialog loop and wait for the user to close the
    dialog.'''
    self._dialog.run(wait=True)


  def close(self):
    '''Close the dialog window.'''
    self._dialog.close()


  def enableTimeLimit(self, enable):
    '''Enable or disable the time limit spinbox.'''

    if enable:
      self._timeLimitSpin['state'] = ['normal']
      self._timeLimitCheckVar.set('1')
    else:
      self._timeLimitSpin['state'] = ['disabled']
      self._timeLimitCheckVar.set('0')


  def setTimeLimit(self, value):
    '''Set time limit to 'value'. value must be an integral value.'''

    self._timeLimit.set(int(value))

  def _setAlgo(self, value):
    '''Set the algorithm to value.'''

    self._algoVar.set(value)

  def enablePositionLimit(self, enable):
    '''Enable or disable the position limit spinbox.'''

    if enable:
      self._positionLimitSpin['state'] = ['normal']
      self._positionLimitCheckVar.set('1')
    else:
      self._positionLimitSpin['state'] = ['disabled']
      self._positionLimitCheckVar.set('0')


  def setPositionLimit(self, value):
    '''Set position limit to value.'''

    self._positionLimitVar.set(int(value))


  def enableMemLimit(self, enable):
    '''Enable or disable the memory limit spinbox.'''

    if enable:
      self._memLimitSpin['state'] = ['normal']
      self._memLimitCheckVar.set('1')
    else:
      self._memLimitSpin['state'] = ['disabled']
      self._memLimitCheckVar.set('0')


  def setMemLimit(self, value):
    '''Set the memory limit to a value.'''

    self._memLimitVar.set(int(value))


  def setTransTblSize(self, value):
    '''Set the transposition table size spinbox to the given value.'''

    self._memorySpinVar.set(int(value))


  def setProofTblSize(self, value):
    '''Set the proof table size spinbox to the given value.'''

    self._proofSpinVar.set(int(value))


  def setKillerCount(self, value):
    '''Set the killer count for each level.'''
    
    self._killerCountVar.set(int(value))

  
  def setMoveCache(self, value):
    '''Set the move cache size.'''
    
    self._moveCacheSpinVar.set(int(value))
  
  
  def enableGC(self, enable):
    '''Enable or disable garbage collector.'''
    
    if enable:
      self._gcHighSpin['state'] = ['normal']
      self._gcLowSpin['state'] = ['normal']
      self._gcCheckVar.set('1')
    else:
      self._gcHighSpin['state'] = ['disabled']
      self._gcLowSpin['state'] = ['disabled']
      self._gcCheckVar.set('0')
  
  
  def setGCHigh(self, val):
    self._gcHigh.set(val)
  
  def setGCLow(self, val):
    self._gcLow.set(val)
  
  
  def enableLog(self, enable):
    if enable:
      self._logCheckVar.set('1')
      state = ['normal']
    else:
      self._logCheckVar.set('0')
      state = ['disabled']
    
    self._logPathName['state'] = state
    self._logPathChooseBtn['state'] = state
  
  def setLogPathName(self, name):
    self._logPathNameVar.set(name)
    

  def _execute(self):
    '''User has clicked the 'Run' button: Dispatch the command to the 
    controller.
    '''

    self.notifyObservers(command=RunSearchDialog.Command.start)


class RunSearchController(object):
  '''Controller of RunSearchDialog. If the user chooses to execute the search,
  it will start the computation and show a new dialog showing the progress.
  '''

  algo = property(lambda self: self._algo)
  timeLimit = property(lambda self: self._timeLimit)
  posLimit = property(lambda self: self._posLimit)
  memLimit = property(lambda self: self._memLimit)
  transTblSize = property(lambda self: self._transTblSize)
  proofTblSize = property(lambda self: self._proofTblSize)
  moveCacheSize = property(lambda self: self._moveCacheSize)
  showTimeLeft = property(lambda self: self._showTimeLeft)

  def __init__(self, runSearchDlg, parameters):
    '''Create the controller and attach it to the dialog and search.
    '''
    object.__init__(self)

    self._runSearchDlg = runSearchDlg

    # Only enable the memory limit by default on 32-bit systems
    is64Bit = sys.maxsize > 2**32  # Trick straight from the docs.

    def get(name, default):
      v = parameters.__dict__.get(name, None)
      if v is not None:
        return v
      else:
        return default

    self._runSearchDlg.addObserver(self)
    self._runSearchDlg.algo = get('algo', apnsmod.algos.keys()[0])
    self._runSearchDlg.timeLimit = get('timeLimit', 60)
    self._runSearchDlg.timeLimitCheck = get('timeLimitCheck', True)
    self._runSearchDlg.positionLimit = get('positionLimit', 1000000)
    self._runSearchDlg.positionLimitCheck = get('positionLimitCheck', False)
    self._runSearchDlg.memLimit = get('memLimit', 1500)
    self._runSearchDlg.memLimitCheck = get('memLimitCheck', not is64Bit)
    self._runSearchDlg.transTblSize = get('transTblSize', 32)
    self._runSearchDlg.proofTblSize = get('proofTblSize', 32)
    self._runSearchDlg.killerCount = get('killerCount', 2)
    self._runSearchDlg.gcHigh = get('gcHigh', '5000000')
    self._runSearchDlg.gcLow = get('gcLow', '3000000')
    self._runSearchDlg.gcCheck = get('gcCheck', True)
    self._runSearchDlg.logCheck = get('logCheck', False)
    self._runSearchDlg.logPathName = get('logPath', 'log.txt')
    #self._runSearchDlg.moveCacheSize = get('moveCacheSize', 32)

    self._doRun = False
    self._lastSetValues = None


  def run(self):
    '''ctrl.run() -> (doRun, lastValues)

    Execute the dialog. Return a boolean value indicating whether the search
    should be performed or not, and a new dictionary of last set values of None
    if last set values should not be updated.
    '''

    self._runSearchDlg.run()
    return (self._doRun, self._lastSetValues)


  def update(self, source, command):
    '''React to user input from the dialog.'''

    MB = 1024 ** 2  # Megabyte.

    if command == RunSearchDialog.Command.start:
      if self._validateInput():
        self._runSearchDlg.close()

        showTimeLeft = self._runSearchDlg.timeLimitCheck
        if showTimeLeft:
          sTimeLimit = int(self._runSearchDlg.timeLimit)
        else:
          sTimeLimit = None

        if self._runSearchDlg.positionLimitCheck:
          posLimit = int(self._runSearchDlg.positionLimit)
        else:
          posLimit = None

        if self._runSearchDlg.memLimitCheck:
          memLimit = int(self._runSearchDlg.memLimit)
        else:
          memLimit = None

        self._algo = self._runSearchDlg.algo
        self._transTblSize = (int(self._runSearchDlg.transTblSize) * MB) / \
                             apnsmod.TranspositionTable.sizeOfElement
        self._proofTblSize = (int(self._runSearchDlg.proofTblSize) * MB) / \
                             apnsmod.ProofTable.sizeOfElement
        self._timeLimit = sTimeLimit
        self._posLimit = posLimit
        self._memLimit = memLimit
        self._showTimeLeft = showTimeLeft
        self._killerCount = self._runSearchDlg.killerCount
        self._gcHigh = self._runSearchDlg.gcHigh
        self._gcLow = self._runSearchDlg.gcLow
        self._gcCheck = self._runSearchDlg.gcCheck
        self._logCheck = self._runSearchDlg.logCheck
        self._logPath = self._runSearchDlg.logPathName
        #self._moveCachesize = self._runSearchDlg.moveCacheSize

        self._lastSetValues = SearchParameters()
        last = self._lastSetValues

        last.algo = self._algo
        last.timeLimitCheck = bool(showTimeLeft)
        last.timeLimit = int(self._runSearchDlg.timeLimit)
        last.positionLimit = int(self._runSearchDlg.positionLimit)
        last.positionLimitCheck = bool(self._runSearchDlg.positionLimitCheck)
        last.memLimit = int(self._runSearchDlg.memLimit)
        last.memLimitCheck = bool(self._runSearchDlg.memLimitCheck)
        last.transTblSize = int(self._runSearchDlg.transTblSize)
        last.proofTblSize = int(self._runSearchDlg.proofTblSize)
        last.killerCount = self._runSearchDlg.killerCount
        last.gcHigh = self._runSearchDlg.gcHigh
        last.gcLow = self._runSearchDlg.gcLow
        last.gcCheck = self._runSearchDlg.gcCheck
        last.logCheck = self._runSearchDlg.logCheck
        last.logPath = self._runSearchDlg.logPathName
        #last.moveCacheSize = int(self._runSearchDlg.moveCacheSize)

        self._doRun = True
        self._runSearchDlg.close()

    elif command == RunSearchDialog.Command.timeLimitCheck:
      self._runSearchDlg.enableTimeLimit(self._runSearchDlg.timeLimitCheck)

    elif command == RunSearchDialog.Command.positionLimitCheck:
      self._runSearchDlg.enablePositionLimit(
        self._runSearchDlg.positionLimitCheck
      )

    elif command == RunSearchDialog.Command.memLimitCheck:
      self._runSearchDlg.enableMemLimit(self._runSearchDlg.memLimitCheck)
    
    elif command == RunSearchDialog.Command.gcCheck:
      self._runSearchDlg.enableGC(self._runSearchDlg.gcCheck)
    
    elif command == RunSearchDialog.Command.logCheck:
      self._runSearchDlg.enableLog(self._runSearchDlg.logCheck)
    
    elif command == RunSearchDialog.Command.logChoose:
      pathname = tkFileDialog.asksaveasfilename()
      if len(pathname) > 0:
        self._runSearchDlg.setLogPathName(pathname)
      

  def _validateInput(self):
    '''Check whether the values set on the dialog are correct. If they are,
    return True; if they aren't, show an error box and return False.
    '''

    def showMustBeInteger(name):
      tkMessageBox.showerror('Invalid value',
                             '%s must be a positive integer' % name)

    def checkVar(check, var, name):
      if check and not var.strip().isdigit():
        showMustBeInteger(name)
        return False
      else:
        return True

    dlg = self._runSearchDlg

    return (checkVar(dlg.timeLimitCheck, dlg.timeLimit, 'Time limit')
            and checkVar(dlg.positionLimitCheck, dlg.positionLimit,
                         'Position limit')
            and checkVar(dlg.memLimitCheck, dlg.memLimit, 'Memory limit')
            and checkVar(True, dlg.transTblSize, 'Size of transposition table')
            and checkVar(True, dlg.proofTblSize, 'Size of proof table')
            and checkVar(True, dlg.killerCount, 'Killer count')
            and checkVar(dlg.gcCheck, dlg.gcHigh, 'GC high threshold')
            and checkVar(dlg.gcCheck, dlg.gcLow, 'GC low threshold'))
            #and checkVar(True, dlg.moveCacheSize, 'Move cache size'))


class SearchProgressDialog(Observable):
  '''A dialog window showing the progress of a search along with a Cancel
  button.
  '''
  
  parent = property(lambda self: self._dialog.parent)

  def __init__(self, parent, showTimeLeft=True, running=True):
    '''Create the dialogue.

    If running is True, the dialog should display strings informing the user
    that the computation is in progress. Otherwise, it'll inform the user that
    it's displaying the statistics of a previous run.
    '''

    Observable.__init__(self)

    if running:
      self._dialog = DialogWindow(parent, 'Expanding tree...')
    else:
      self._dialog = DialogWindow(parent, 'Statistics')

    if running:
      infoLabel = ttk.Label(self._dialog.content, text='Computation is now in progress. Please wait.')
    else:
      infoLabel = ttk.Label(self._dialog.content, text='Last search statistics:')

    stats = ttk.Labelframe(self._dialog.content, text='Statistics:', padding=3)

    memoryAllocLbl      = ttk.Label(stats, text='Tree memory usage:')
    transTblSizeLbl     = ttk.Label(stats, text='Transposition table size:')
    transTblHitsLbl     = ttk.Label(stats, text='Transposition table hits:')
    transTblMissesLbl   = ttk.Label(stats, text='Transposition table misses:')
    proofTblSizeLbl     = ttk.Label(stats, text='Proof table size:')
    proofTblHitsLbl     = ttk.Label(stats, text='Proof table hits:')
    proofTblMissesLbl   = ttk.Label(stats, text='Proof table misses:')
    killerCountLbl      = ttk.Label(stats, text='Total killer count:')
    posCountLbl         = ttk.Label(stats, text='Vertex count:')
    posPerSecLbl        = ttk.Label(stats, text='New vertices per second:')

    self._memoryAlloc     = ttk.Label(stats)
    self._transTblSize    = ttk.Label(stats)
    self._transTblHits    = ttk.Label(stats)
    self._transTblMisses  = ttk.Label(stats)
    self._proofTblSize    = ttk.Label(stats)
    self._proofTblHits    = ttk.Label(stats)
    self._proofTblMisses  = ttk.Label(stats)
    self._killerCount     = ttk.Label(stats)
    self._posCount        = ttk.Label(stats)
    self._posPerSec       = ttk.Label(stats)

    self._cancelBtn = ttk.Button(
      self._dialog.buttonBox,
      text='Cancel' if running else 'OK', command=self._cancel
    )
    self._dialog.setDeleteAction(lambda: self._cancelBtn.invoke())

    progress = ttk.Labelframe(self._dialog.content, text='Progress:' if running else 'Time:', padding=3)

    rootPnLabel = ttk.Label(progress, text='Root PN:')
    rootDnLabel = ttk.Label(progress, text='Root DN:')
    self._rootPn = ttk.Label(progress)
    self._rootDn = ttk.Label(progress)

    timeElapsedLabel = ttk.Label(progress, text='Time elapsed:')
    self._timeElapsed = ttk.Label(progress)

    if running:
      rootPnLabel.grid(row=0, column=0, sticky='E')
      rootDnLabel.grid(row=1, column=0, sticky='E')
      self._rootPn.grid(row=0, column=1, sticky='W', padx=(5, 0))
      self._rootDn.grid(row=1, column=1, sticky='W', padx=(5, 0))
      timeElapsedLabel.grid(row=2, column=0, sticky='E')
      self._timeElapsed.grid(row=2, column=1, sticky='W', padx=(5, 0))
    else:
      timeElapsedLabel.grid(row=0, column=0, sticky='E')
      self._timeElapsed.grid(row=0, column=1, sticky='W', padx=(5, 0))

    if showTimeLeft:
      timeLeftLabel = ttk.Label(progress, text='Time left:')
      self._timeLeft = ttk.Label(progress)
      timeLeftLabel.grid(row=3, column=0, sticky='E')
      self._timeLeft.grid(row=3, column=1, sticky='W', padx=(5, 0))

    infoLabel.grid(row=0, column=0, sticky='W', pady=5)
    stats.grid(row=1, column=0, sticky='WE', pady=5)
    progress.grid(row=2, column=0, sticky='WE', pady=5)

    memoryAllocLbl.grid(row=0, column=0, sticky='E')
    transTblSizeLbl.grid(row=1, column=0, sticky='E')
    transTblHitsLbl.grid(row=2, column=0, sticky='E')
    transTblMissesLbl.grid(row=3, column=0, sticky='E')
    proofTblSizeLbl.grid(row=4, column=0, sticky='E')
    proofTblHitsLbl.grid(row=5, column=0, sticky='E')
    proofTblMissesLbl.grid(row=6, column=0, sticky='E')
    killerCountLbl.grid(row=7, column=0, sticky='E')
    posCountLbl.grid(row=8, column=0, sticky='E')
    posPerSecLbl.grid(row=9, column=0, sticky='E')

    self._memoryAlloc.grid(row=0, column=1, sticky='W', padx=(5, 0))
    self._transTblSize.grid(row=1, column=1, sticky='W', padx=(5, 0))
    self._transTblHits.grid(row=2, column=1, sticky='W', padx=(5, 0))
    self._transTblMisses.grid(row=3, column=1, sticky='W', padx=(5, 0))
    self._proofTblSize.grid(row=4, column=1, sticky='W', padx=(5, 0))
    self._proofTblHits.grid(row=5, column=1, sticky='W', padx=(5, 0))
    self._proofTblMisses.grid(row=6, column=1, sticky='W', padx=(5, 0))
    self._killerCount.grid(row=7, column=1, sticky='W', padx=(5, 0))
    self._posCount.grid(row=8, column=1, sticky='W', padx=(5, 0))
    self._posPerSec.grid(row=9, column=1, sticky='W', padx=(5, 0))

    stats.columnconfigure(1, weight=1)
    self._dialog.content.columnconfigure(0, weight=1)

    self._cancelBtn.grid(row=0, column=0, sticky='E')
    self._cancelBtn.focus_set()

    self.showMemoryAllocated('0 B')
    self.showTransTblStats(0, 0, 0)
    self.showProofTblStats(0, 0, 0)
    self.showPosCount(0, 0)

    self._dialog.window.bind('<Escape>', lambda e: self._cancel())


  def run(self):
    '''Show the dialog on-screen, but do not block until it closes.'''

    self._dialog.run(wait=False)


  def close(self):
    '''Dismiss the dialog window.'''

    self._dialog.close()


  def updateGui(self):
    '''Run the Tkinter event loop to keep the GUI responsible.'''

    self._dialog.window.update()


  def showTimeLeft(self, sTime):
    '''Show how long is the search still going to take, in seconds.'''

    self._timeLeft['text'] = '%d seconds' % sTime


  def showTimeElapsed(self, sTime):
    '''Show how much time has passed since the start of the calculation, in
    seconds.
    '''

    self._timeElapsed['text'] = '%d seconds' % sTime


  def showMemoryAllocated(self, allocated):
    '''Show how much memory has been allocated for vertices.'''

    self._memoryAlloc['text'] = '%s' % (allocated)


  def showTransTblStats(self, memUsed, hits, misses):
    '''Show statistics about the transposition table.'''

    self._transTblSize['text'] = '%s' % memUsed
    self._transTblHits['text'] = '%s' % hits
    self._transTblMisses['text'] = '%s' % misses


  def showProofTblStats(self, memUsed, hits, misses):
    '''Show statistics about the proof table.'''

    self._proofTblSize['text'] = '%s' % memUsed
    self._proofTblHits['text'] = '%s' % hits
    self._proofTblMisses['text'] = '%s' % misses
  

  def showKillerCount(self, count):
    self._killerCount['text'] = '%s' % count


  def showPosCount(self, posCount, posPerSec):
    '''Show statistics about the number of positions considered.'''

    self._posCount['text'] = '%s' % posCount
    self._posPerSec['text'] = '%s' % int(posPerSec)


  def showRootPnDn(self, pn, dn):

    self._rootPn['text'] = '%d' % pn
    self._rootDn['text'] = '%d' % dn


  def _cancel(self):
    self.notifyObservers()
    

class AllocatingDialog(object):
  '''A dialog with just the message 'Allocating Memory.' '''
  
  def __init__(self, parent):
    object.__init__(self)
    
    self._dlg = DialogWindow(parent, 'Allocating...')
    
    message = ttk.Label(self._dlg.content, text='Allocating Tables...')
    message.pack()
    
  def run(self):
    self._dlg.window.update()
    self._dlg.run(wait=False)
  
  def close(self):
    self._dlg.close()


class SearchProgressController(object):
  '''A controller of SearchProgressDialog.'''

  def __init__(self, searchProgressDlg, timeLimit, posLimit, memLimit):
    object.__init__(self)

    self._searchProgressDlg = searchProgressDlg
    self._allocDialog = None

    self._timeLimit = timeLimit
    self._posLimit = posLimit
    self._memLimit = memLimit
    self._cancel = False


  def run(self, controller):
    '''Run the search. This call will only return after the search is either
    finished or the user has decided to cancel it.

    Returns a dictionary with the statistics, that can later be fed into
    PositionStatsController.
    '''

    MS_BURST_TIME = 250
    
    self._searchProgressDlg.run()

    class Updater:
      def __init__(self, dialog, progCtrl):
        self.dlg = dialog
        self.progCtrl = progCtrl

      def update(self, source):
        '''This is an event from SearchProgressDialog -- the only event it ever
        fires up is the Cancel event.
        '''
        controller.cancel()

      def updateDlg(self, controller, progress):
        dlg = self.dlg
        if self.progCtrl._timeLimit is not None:
          dlg.showTimeLeft(progress.timeLeft)
        dlg.showTimeElapsed(progress.timeElapsed)

        if progress.memUsed < 1024:
          memUsed = str(progress.memUsed)
        elif progress.memUsed < 1024 * 1024:
          memUsed = '{0:.2f} kB'.format(progress.memUsed / float(1024))
        else:
          memUsed = '{0:.2f} MB'.format(progress.memUsed / float(1024 * 1024))
        dlg.showMemoryAllocated(memUsed)

        if progress.transTblSize is not None:
          ttSize = '{0:.2f} MB'.format(progress.transTblSize /
                                                           float(1024 * 1024))
          dlg.showTransTblStats(memUsed=ttSize, hits=progress.transTblHits,
                                misses=progress.transTblMisses)
        else:
          dlg.showTransTblStats(memUsed='0 B', hits='0', misses='0')

        if progress.proofTblSize is not None:
          ptSize = '{0:.2f} MB'.format(progress.proofTblSize /
                                                           float(1024 * 1024))
          dlg.showProofTblStats(memUsed=ptSize, hits=progress.proofTblHits,
                                misses=progress.proofTblMisses)
        else:
          dlg.showProofTblStats(memUsed='0 B', hits='0', misses='0')

        dlg.showKillerCount(count=progress.killerCount)

        dlg.showPosCount(posCount=progress.positionCount, posPerSec=progress.positionsPerSecond)
        dlg.showRootPnDn(progress.rootPN, progress.rootDN)
        dlg.updateGui()

    updater = Updater(self._searchProgressDlg, self)
    self._searchProgressDlg.addObserver(updater)
    controller.searchProgressCallbacks.add(updater.updateDlg)
    
    def stateChanged(ctrl, newState):
      if newState == Controller.State.ALLOCATING:
        if self._allocDialog is None:
          self._allocDialog = AllocatingDialog(self._searchProgressDlg.parent)
          self._allocDialog.run()
          
      elif self._allocDialog is not None:
        self._allocDialog.close()
        self._allocDialog = None
        
    controller.stateCallbacks.add(stateChanged)

    try:
      controller.runSearch(MS_BURST_TIME)
      
    except MemoryError:
      if controller.state == Controller.State.ALLOCATING:
        tkMessageBox.showerror('Error', 'There is not enough memory to allocate the proof or transposition table.')
      else:
        raise
      
    except RuntimeError, e:
      tkMessageBox.showerror(
        'Error',
        'An internal error has occured. This is likely a bug.\n\n{0}'.format(e)
      )
      
    except ValueError, e:
      tkMessageBox.showerror('Error', str(e))
      
    finally:
      controller.searchProgressCallbacks.remove(updater.updateDlg)
      controller.stateCallbacks.remove(stateChanged)
      if self._allocDialog is not None:
        self._allocDialog.close()
        self._allocDialog = None
      self._searchProgressDlg.close()


class SearchStatsController(object):
  '''Another controller for SearchProgressDialog -- this one, however, doesn't
  run the search, but merely displays the stats.
  '''

  def __init__(self, searchProgressDlg, stats):
    s = stats
    searchProgressDlg.showTimeElapsed(s.timeElapsed)
    searchProgressDlg.showMemoryAllocated('{0:.2f} MB'.format(s.memUsed /
                                                          float(1024 * 1024)))
    searchProgressDlg.showPosCount(posCount=s.positionCount, 
                                   posPerSec=s.positionsPerSecond)
    searchProgressDlg.showRootPnDn(s.rootPN, s.rootDN)

    if s.transTblSize is not None:
      ttSize = '{0:.2f} MB'.format(s.transTblSize / float(1024 * 1024))
      searchProgressDlg.showTransTblStats(ttSize, hits=s.transTblHits,
                                          misses=s.transTblMisses)
    else:
      searchProgressDlg.showTransTblStats('0 B', '0', '0')

    if s.proofTblSize is not None:
      ptSize = '{0:.2f} MB'.format(s.proofTblSize / float(1024 * 1024))
      searchProgressDlg.showProofTblStats(ptSize, hits=s.proofTblHits,
                                          misses=s.proofTblMisses)
    else:
      searchProgressDlg.showProofTblStats('0 B', '0', '0')
      
    searchProgressDlg.showKillerCount(count=s.killerCount)

    searchProgressDlg.addObserver(self)
    searchProgressDlg.run()


  def update(self, source):
    '''The dialog has fired up an event. The only event is the OK button, so
    dismiss the dialog.
    '''

    source.close()


class PositionEditorDialog(object):
  '''A dialog window that lets the user modify a game position. It is a modal
  dialog, once created it will wait in its own inner event loop until the user
  closes it. The caller may then check wheter the user has decided to confirm
  or cancel their choice by testing if the .board property is None or not.
  '''

  board = property(lambda self: self._boardController.board)
  player = property(lambda self: self._positionDisplay.color)

  def __init__(self, parent, title, description, imageManager, board=None):
    '''Create the dialog and wait for the user to close it.

    title -- title of the dialog window
    description -- a short description text that will be displayed on the
      dialog,
    imageManager -- an ImageManager instance
    board -- either a Board instance that will be shown to the user or None, in
      which case the user will be initially presented with an empty board.
    '''

    object.__init__(self)

    if board is None:
      board = apnsmod.Board()

    self._parent = parent
    self._window = DialogWindow(parent, title)

    descriptionLabel = ttk.Label(self._window.content, text=description,
                                 padding=(0, 0, 0, 5))
    self._positionDisplay = PositionDisplay(self._window.content, board,
                                            imageManager)
    okButton = ttk.Button(self._window.buttonBox, text='OK', 
                          command=self._window.close)
    cancelButton = ttk.Button(self._window.buttonBox, text='Cancel',
                              command=self._cancel)

    self._boardController = BoardController(
      board, self._positionDisplay.boardDisplay,
      self._positionDisplay.pieceChooser
    )

    self._positionController = PositionController(
      self._window.window, self._positionDisplay, self._boardController
    )

    descriptionLabel.grid(row=0, column=0, sticky='W')
    self._positionDisplay.widget.grid(row=1, column=0, sticky='NSEW')

    self._window.content.columnconfigure(0, weight=1)
    self._window.content.rowconfigure(1, weight=1)

    okButton.grid(row=0, column=0, padx=5)
    cancelButton.grid(row=0, column=1)
    self._window.buttonBox.rowconfigure(0, weight=1)

    okButton.focus_set()
    self._window.window.bind('<Return>', lambda e: self._window.close())
    self._window.window.bind('<Escape>', lambda e: self._cancel)
    self._window.setDeleteAction(lambda: self._cancel())

    self._window.run()


  def _cancel(self):
    '''Handle the Cancel command. Just make .board None and quit the dialog.'''

    self._boardController.detachFromBoard()
    self._window.close()


class PositionDisplay(Observable):
  '''Displays pieces on the _board as well as the piece selector.'''
  widget = property(lambda self: self._frame)

  boardDisplay = property(lambda self: self._boardDisplay)
  pieceChooser = property(lambda self: self._pieceChooser)

  # color = property(...)  <-- Yes, it's here; it's just actually defined at 
  # the bottom.

  class Command:
    '''Command invoked by the user. Either load position, savePosition
    position, clear position or switch player to move.
    '''
    load, savePosition, clear, switch = range(4)


  def __init__(self, parent, board, imageManager):
    '''Create the display.'''
    Observable.__init__(self)

    self._frame = ttk.Frame(parent)

    self._toolbar = ttk.Frame(self._frame)
    self._loadButton = ttk.Button(self._toolbar, text='Load')
    self._saveButton = ttk.Button(self._toolbar, text='Save')
    self._clearButton = ttk.Button(self._toolbar, text='Clear')

    goldSilverFrame = ttk.Frame(self._frame)
    self._color = Tkinter.StringVar(goldSilverFrame, 'g')
    playerLabel = ttk.Label(goldSilverFrame, text='Attacker:')
    self._gold = ttk.Radiobutton(goldSilverFrame, text='Gold', value='g',
                                 variable=self._color)
    self._silver = ttk.Radiobutton(goldSilverFrame, text='Silver', value='s',
                                   variable=self._color)

    self._loadButton['command'] = lambda: self.notifyObservers(
      cmd=PositionDisplay.Command.load)
    self._saveButton['command'] = lambda: self.notifyObservers(
      cmd=PositionDisplay.Command.savePosition)
    self._clearButton['command'] = lambda: self.notifyObservers(
      cmd=PositionDisplay.Command.clear)

    self._boardDisplay = BoardDisplay(self._frame, imageManager)
    self._pieceChooser = PieceChooser(self._frame, imageManager)

    self._loadButton.grid(row=0, column=0, padx=(0, 3))
    self._saveButton.grid(row=0, column=1, padx=(3, 3))
    self._clearButton.grid(row=0, column=2, padx=(3, 0))

    playerLabel.grid(row=0, column=0, padx=(0, 5))
    self._gold.grid(row=0, column=1, padx=(0, 3))
    self._silver.grid(row=0, column=2, padx=(3, 0))

    self._toolbar.grid(row=0, column=0, columnspan=2, sticky='W', pady=(0, 5))
    goldSilverFrame.grid(row=1, column=0, columnspan=2, sticky='E')
    self._boardDisplay.widget.grid(row=2, column=1, sticky='NSEW')
    self._pieceChooser.widget.grid(row=2, column=0, rowspan=1, sticky='NS')

    self._frame.columnconfigure(0, weight=1, pad=5)


  def disable(self):
    '''Disable this widget. Clicking any part of it won't result in any
    command.
    '''
    self._gold.state(['disabled'])
    self._silver.state(['disabled'])


  def enable(self):
    '''Enable the widget. It will then be again sensitive to user input.'''
    self._gold.state(['!disabled'])
    self._silver.state(['!disabled'])


  def selectPlayer(self, player):
    '''Change the player selection to the given player.'''

    if player == apnsmod.Piece.Color.gold:
      self._gold.invoke()
    else:
      self._silver.invoke()


  def _getColor(self):
    '''Get the Piece.Color value of the currently selected player.'''

    if self._color.get() == 'g':
      return apnsmod.Piece.Color.gold
    else:
      return apnsmod.Piece.Color.silver

  color = property(_getColor)


class PositionController(object):
  '''Controller of position display.'''

  board = property(lambda self: self._boardController.board)

  def __init__(self, parent, positionDisplay, boardController):
    '''Initialise a controller.'''

    object.__init__(self)

    self._positionDisplay = positionDisplay
    self._boardController = boardController
    self._parent = parent

    self._positionDisplay.addObserver(self)


  def update(self, source, cmd):
    '''Update the controller's state if the user invoked any GUI action.'''

    if cmd == PositionDisplay.Command.savePosition:
      filename = tkFileDialog.asksaveasfilename(parent=self._parent)

      if len(filename) > 0:
        try:
          saveBoard(
            self._boardController._board, 1, 
            'g' if self._positionDisplay.color == apnsmod.Piece.Color.gold \
                else 's',
            filename
          )
        except RuntimeError, e:
          tkMessageBox.showerror('Could not write to file: %s' % e.what())

    elif cmd == PositionDisplay.Command.load:
      filename = tkFileDialog.askopenfilename(parent=self._parent)

      if len(filename) > 0:
        try:
          (newBoard, player) = loadBoard(filename)
          self._boardController.attachToBoard(newBoard)
          self._boardController.enable()
          self._positionDisplay.selectPlayer(player)
        except RuntimeError, e:
          tkMessageBox.showerror('Error', 'Could not open file: %s' % e)

    elif cmd == PositionDisplay.Command.clear:
      self._boardController.clearBoard()


class ImageManager(object):
  '''ImageManager merely loads and stores images. It is a class of its own to
  solve three problems:

  1) Images may only be loaded after an instance of Tk has been constructed;
  2) Images must be destroyed before Tk is destroyed;
  3) Tkinter doesn't hold references to loaded images -- something else, then,
    needs to hold them.

  Therefore the purpose of this class is to load images upon creation, hold
  references to them so that they stay alive, and ensure destruction at the
  "right time" -- that is, when individual parts of the interface are
  destroyed.

  The destruction part is implicit. The only important thing is that the last
  references to the loaded images are dropped when objects are being destroyed.
  If the references were held on a module level, the destruction would happen
  *after* the destruction of the Tk object.

  An instance of this class may only be created after an instance of Tk has
  been created.
  '''

  background = property(lambda self: self._background)
  # Dictionary (color, type) -> Image
  pieceImages = property(lambda self: self._pieceImages)  
  selection = property(lambda self: self._selection)
  removeImage = property(lambda self: self._removeImage)

  def __init__(self):
    '''Load images from disk. If there is an error, display an error box and
    raise SystemExit. Tk needs to have been constructed.
    '''

    object.__init__(self)

    try:
      from Tkinter import PhotoImage
      self._background = PhotoImage(
        file='interface/arimaa-graphics/BoardMarbleSmall.gif')
      self._selection = PhotoImage(
        file='interface/arimaa-graphics/selection.gif')
      self._removeImage = PhotoImage(
        file='interface/arimaa-graphics/remove.gif')

      self._pieceImages = dict()
      for color in _COLORS:
        for type_ in _TYPES:
          img = PhotoImage(file=self._imageName(color, type_))
          self._pieceImages[(color, type_)] = img

    except Tkinter.TclError, e:
      tkMessageBox.showerror('Error', 'Failed to load graphics.\n%s' % e)
      raise SystemExit(1)


  def _imageName(self, color, type_):
    '''Get the filename of the image of the piece with given color and type.'''

    colorName = {
      apnsmod.Piece.Color.gold: 'Gold',
      apnsmod.Piece.Color.silver: 'Silver' 
    }[color]

    typeName = {
      apnsmod.Piece.Type.elephant: 'Elephant',
      apnsmod.Piece.Type.camel: 'Camel',
      apnsmod.Piece.Type.horse: 'Horse',
      apnsmod.Piece.Type.dog: 'Dog',
      apnsmod.Piece.Type.cat: 'Cat',
      apnsmod.Piece.Type.rabbit: 'Rabbit' 
    }[type_]

    return 'interface/arimaa-graphics/' + colorName + typeName + '.gif'


class PieceChooser(Observable):
  '''Widget to let a user choose a piece. When a user clicks on a piece, this
  object fires an even through the Observable interface with two keyword
  parameters: color and type.
  '''

  widget = property(lambda self: self._frame)

  _HILIGHT_COLOR = '#CC5500'
  _NORMAL_COLOR = None  # Set in __init__.

  def __init__(self, parent, imageManager):
    Observable.__init__(self)

    self._frame = ttk.LabelFrame(parent, text='Add/Remove:')
    self._frame['padding'] = 5

    self._pieces = dict()

    for column in xrange(0, len(_COLORS)):
      for row in xrange(0, len(_TYPES)):
        color = _COLORS[column]
        type_ = _TYPES[row]

        b = ttk.Label(self._frame,
                      image=imageManager.pieceImages[color, type_])
        b.color = color
        b.type = type_
        b.bind('<1>', self._pieceClicked)

        b.grid(row=row, column=column)
        self._pieces[color, type_] = b

        if PieceChooser._NORMAL_COLOR is None:
          PieceChooser._NORMAL_COLOR = b['background']

    remove = ttk.Label(self._frame, image=imageManager.removeImage)
    remove.color = None
    remove.type = None
    remove.bind('<1>', self._pieceClicked)
    remove.grid(row=len(_TYPES) + 1, column=0, columnspan=2)
    self._pieces[None, None] = remove


  def _pieceClicked(self, event):
    widget = event.widget
    color = widget.color
    type_ = widget.type

    self.notifyObservers(color=color, type=type_)


  def select(self, color, type_):
    '''Hilight the piece of the given color and type in the selection.'''
    self._pieces[color, type_]['background'] = PieceChooser._HILIGHT_COLOR


  def unselect(self, color, type_):
    '''De-hilight the piece of the given color and type.'''
    self._pieces[color, type_]['background'] = PieceChooser._NORMAL_COLOR


class BoardDisplay(Observable):
  '''Displays the game _board. On click, this widgets fires an event through
  the Observable interface. The event contains two keyword arguments: row and
  column which specify which position on the _board was clicked on.
  '''

  widget = property(lambda self: self._canvas)

  def __init__(self, parent, imageManager):
    Observable.__init__(self)
    self._imageManager = imageManager

    self._canvas = Tkinter.Canvas(parent, width=_BOARD_WIDTH, 
                                  height=_BOARD_HEIGHT)
    self._canvas.create_image(0, 0, image=imageManager.background, anchor='nw')
    self._canvas.bind('<1>', self._onClick)


  def select(self, row, column):
    '''Draw a selection rectangle around the given row and column. Return the
    selection object.

    The returned selection object is of an unspecified type except that the
    same object must be passed to unselect to remove the selection.
    '''

    x, y = self._rowColumnToXY(row, column)
    return self._canvas.create_image(x, y, image=self._imageManager.selection,
                                     anchor='nw')


  def unselect(self, selection):
    '''Remove the given selection. 'selection' is the object returned from the
    select function.
    '''

    self._canvas.delete(selection)


  def putPiece(self, row, column, color, type_):
    '''Put the specified piece on the given row and column. Return a handle to
    the piece. The returned handle must then be passed to removePiece. It is of
    an unspecified type. This function doesn't check if the target position is
    empty or not.
    '''

    x, y = self._rowColumnToXY(row, column)
    img = self._imageManager.pieceImages[color, type_]

    return self._canvas.create_image(x, y, image=img, anchor='nw')


  def removePiece(self, handle):
    '''Remove a previously added piece from the display. 'handle' is the value
    returned from the addPiece call.
    '''

    self._canvas.delete(handle)


  def _onClick(self, event):
    x, y = event.x, event.y
    row, column = self._xyToRowColumn(x, y)
    self.notifyObservers(row=row, column=column)


  def _rowColumnToXY(self, row, column):
    '''Convert (row, column) coordinates into (x, y) coordinates of the board
    background image. The resulting value represents some point inside
    specified square.
    '''

    x = _OUTER_BORDER + column * (_INNER_BORDER + _TILE_WIDTH)
    y = _OUTER_BORDER + row * (_INNER_BORDER + _TILE_HEIGHT)
    return (x, y)


  def _xyToRowColumn(self, x, y):
    '''Convert (x, y) coordinates of the board background image to the (row,
    column) coordinates of the square under that pixel.
    '''

    column = (x - _OUTER_BORDER) / (_INNER_BORDER + _TILE_WIDTH)
    row = (y - _OUTER_BORDER) / (_INNER_BORDER + _TILE_HEIGHT)
    return (row, column)


class BoardController(object):
  '''Control logic of the BoardDisplay and PieceChooser widgets. An instance of
  BoardDisplay must always be specified; an instance of PieceDisplay may be
  omitted, in which case the controller assumes that there is no corresponding
  PieceChooser object and so new pieces may not be added to the BoardDisplay.  
  '''

  board = property(lambda self: self._board)

  def __init__(self, board, boardDisplay, piecesDisplay):
    '''Create an instance. Parameters:
    _board: an instance of solver.Board to manipulate or None if it should be
    initially detached from any _board

    boardDisplay: the display widget of the _board 
    
    piecesDisplay: an instance of PieceChooser widget or None
    '''

    self._board = board

    self._enabled = False
    if self._board is not None:
      self._enabled = True

    self._boardDisplay = boardDisplay
    self._boardDisplay.addObserver(self)

    self._piecesDisplay = piecesDisplay
    if self._piecesDisplay is not None:
      self._piecesDisplay.addObserver(self)

    self._selectedPiece = None
    self._boardSelection = None
    self._boardSelectionRowColumn = None
    self._displayedPieces = dict()  # Dictionary apnsmod.Position -> handle


  def update(self, source, **kwargs):
    '''React to updates from the associated BoardDisplay or PieceChooser.'''

    if self._enabled:
      if source == self._piecesDisplay:
        color = kwargs['color']
        type_ = kwargs['type']
        self._updatePieceSelection(color, type_)

      elif source == self._boardDisplay:
        row = kwargs['row']
        column = kwargs['column']
        self._updateBoardSelection(row, column)

      else:
        assert False, 'Never gets here'


  def clearBoard(self):
    '''Remove all pieces from the _board.'''
    self._removeBoardSelection()
    self._removePieceSelection()
    for (position, _) in self._board.pieces:
      self._board.remove(position)
      self._updateBoard(position)


  def enable(self):
    '''Become reactive to user input.'''
    self._enabled = True


  def disable(self):
    '''Stop reacting to user input.'''

    self._removeBoardSelection()
    self._removePieceSelection()
    self._enabled = False


  def detachFromBoard(self):
    '''Discard the reference to the Board object. The display will show an
    empty _board and will not react to any user input.'''

    for displayedPiece in self._displayedPieces.values():
      self._boardDisplay.removePiece(displayedPiece)

    self._displayedPieces = {}

    self._board = None
    self.disable()


  def attachToBoard(self, newBoard):
    '''Control a new Board object.'''

    self.detachFromBoard()
    self._board = newBoard

    if self._board is not None:
      for (position, piece) in self._board.pieces:
        self._displayPiece(position, piece)


  def _updatePieceSelection(self, color, type_):
    '''Update selection on the PieceChooser.'''

    if self._selectedPiece is not None and \
        self._selectedPiece == (color, type_):
      self._removePieceSelection()

    else:
      self._removePieceSelection()
      self._removeBoardSelection()

      self._piecesDisplay.select(color, type_)
      self._selectedPiece = (color, type_)


  def _updateBoardSelection(self, row, column):
    '''Update piece selection on the _board.'''

    # If there is a piece selected in the chooser and then the user clicks on a
    # position on the _board, it's an "add piece" command.

    empty = apnsmod.empty
    Position = apnsmod.Position
    Piece = apnsmod.Piece

    if self._selectedPiece is not None:
      r, c = self._boardCoordsFromDisplay(row, column)
      color, type_ = self._selectedPiece

      if empty(Position(r, c), self._board):
        self._board.put(Position(r, c), Piece(color, type_))
        self._updateBoard(Position(r, c))
        self._removePieceSelection()

      elif self._selectedPiece == (None, None) and \
          not empty(Position(r, c), self._board):
        self._board.remove(Position(r, c))
        self._updateBoard(Position(r, c))
        self._removePieceSelection()

    else:
      if self._boardSelection is not None:
        # If there already is a piece selected on the _board, then move it to
        # the new position, if the new position is empty.
        (r, c) = self._boardSelectionRowColumn

        (oldRow, oldColumn) = self._boardCoordsFromDisplay(r, c)
        oldPosition = Position(oldRow, oldColumn)

        (newRow, newColumn) = self._boardCoordsFromDisplay(row, column)
        newPosition = Position(newRow, newColumn)

        if empty(newPosition, self._board):
          piece = self._board.get(oldPosition)
          self._board.remove(oldPosition)
          self._updateBoard(oldPosition)

          self._board.put(newPosition, piece)
          self._updateBoard(newPosition)

          self._removeBoardSelection()

        else:
          self._removeBoardSelection()

      else:
        # If there is no selection, create a new one if there is a piece on the
        # selected place.
        (r, c) = self._boardCoordsFromDisplay(row, column)

        if not empty(Position(r, c), self._board):
          self._boardSelection = self._boardDisplay.select(row, column)
          self._boardSelectionRowColumn = (row, column)


  def _updateBoard(self, position):
    piece = self._board.get(position)
    if piece is not None:  # An addition of a piece
      self._displayPiece(position, piece)

    else:
      self._boardDisplay.removePiece(self._displayedPieces[position])
      del self._displayedPieces[position]


  def _removePieceSelection(self):
    '''Deselect the currently selected piece (if any) in the PieceChooser.'''
    if self._selectedPiece is not None:
      self._piecesDisplay.unselect(self._selectedPiece[0],
                                   self._selectedPiece[1])
      self._selectedPiece = None


  def _removeBoardSelection(self):
    '''Deselect the currently selected piece (if any) on the BoardDisplay.'''
    if self._boardSelection is not None:
      self._boardDisplay.unselect(self._boardSelection)
      self._boardSelection = None
      self._boardSelectionRowColumn = None


  def _displayPiece(self, position, piece):
    '''Add the specified piece to the _board display.'''
    displayRow, displayColumn = self._displayCoordsFromBoard(position.row,
                                                             position.column)
    handle = self._boardDisplay.putPiece(displayRow, displayColumn, 
                                         piece.color, piece.type)
    self._displayedPieces[position] = handle


  def _boardCoordsFromDisplay(self, row, column):
    '''Convert to _board coordinates from display coordinates. Display
    coordinates use numbers 1 to 8 for columns; _board coordinates use letters
    'a' to 'h'.
    '''

    c = chr(ord('a') + column)
    return (8 - row, c)


  def _displayCoordsFromBoard(self, row, column):
    '''Convert to display coordinates from _board coordinates.'''

    c = ord(column) - ord('a')
    return (8 - row, c)


def runGui():
  mainWindowDsply = MainWindow()
  mainWindowCtrl = MainWindowController(mainWindowDsply)
  mainWindowCtrl.runApplication()
  del mainWindowCtrl
  del mainWindowDsply
  gc.collect()

if __name__ == '__main__':
  runGui()
