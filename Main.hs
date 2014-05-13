module Main(main) where





import Engine.Setup
import Game.Setup

main :: IO ()
main = do
  (state, resourcesToLoad) <- setupGame

  -- bit badly named, also starts the game.
  setupEngine 1920 1080 "Welcome to AO2D" state resourcesToLoad
