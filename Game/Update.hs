module Game.Update(updateGame) where

import Data.IORef

import Model.State
import Model.State.Input
import Model.State.Engine
import Model.State.Game

updateGame :: State -> Double -> IO ()
updateGame (gsIO, isIO, esIO) delta = do
  gs <- readIORef gsIO
  is <- readIORef isIO
  es <- readIORef esIO

  print delta
