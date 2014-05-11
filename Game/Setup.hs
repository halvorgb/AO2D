module Game.Setup(setupGame) where


import System.FilePath
import Data.IORef

import Game.LevelGen.LevelGen

import Model.State
import Model.State.Game
import Model.State.Input
import Model.State.Resources
import Model.ShaderProgram



setupGame :: IO (State, Resources)
setupGame = do
  level <- generateLevel w h
  inputState    <- newIORef Nothing
  gameState     <- newIORef $ GameState level undefined
  resourceState <- newIORef $ LoadedResources [] []

  return ((gameState, inputState,resourceState), resourcesToLoad)
    where
      (w,h) = (50,50)

      resourcesToLoad =
          Resources {
              rShaderPrograms = [ShaderProgram "game2d" ("assets" </> "shaders" </> "game2d.vert") ("assets" </> "shaders" </> "game2d.frag"),
                                ShaderProgram "lol" ("assets" </> "shaders" </> "lol.vert") ("assets" </> "shaders" </> "lol.frag")],
              rTextures = []
            }
