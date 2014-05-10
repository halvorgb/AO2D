module Game.Setup(setupGame) where



import Data.IORef

import Game.LevelGen.LevelGen

import Model.State
import Model.State.Game
import Model.State.Engine
import Model.State.Input


setupGame :: IO State
setupGame = do
  level <- generateLevel w h
  inputState  <- newIORef Nothing
  gameState   <- newIORef $ GameState level undefined
  engineState <- newIORef $ EngineState [] [] 0

  return (gameState, inputState, engineState)
    where
      (w,h) = (50,50)
