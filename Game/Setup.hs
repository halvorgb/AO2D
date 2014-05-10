module Game.Setup(setupGame) where

import Game.LevelGen.LevelGen

import Model.GameState

setupGame :: IO (GameState)
setupGame = do
  level <- generateLevel w h
  return $ GameState level undefined
    where
      (w,h) = (50,50)
