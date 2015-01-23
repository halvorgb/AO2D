module Main(main) where

import           Engine.InitEngine
import           Game.Setup

main :: IO ()
main = do
  initialState <- setupGame

  -- bit badly named, also starts the game.
  initEngine 1920 1080 "Welcome to AO2D" initialState
