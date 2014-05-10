module Main(main) where





import Render.Setup
import Game.Setup

main :: IO ()
main = do
  gameState <- setupGame

  setupRenderer
