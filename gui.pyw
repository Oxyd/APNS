#!/usr/bin/env python

from interface.gui import MainWindow, MainWindowController

if __name__ == '__main__':
  mainWindow = MainWindow()
  mainWindowCtrl = MainWindowController(mainWindow)
  mainWindowCtrl.runApplication()
  del mainWindowCtrl
  del mainWindow
