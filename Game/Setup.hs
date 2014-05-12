module Game.Setup(setupGame) where


import System.FilePath
import Data.IORef

import Graphics.Rendering.OpenGL

import Game.LevelGen.LevelGen

import Model.State
import Model.State.Game
import Model.State.Input
import Model.State.Resources
import Model.ShaderProgram


import Lib.LoadShaders (ShaderInfo(..), ShaderSource(..))


setupGame :: IO (State, Resources)
setupGame = do
  level <- generateLevel w h
  inputState    <- newIORef Nothing
  gameState     <- newIORef $ GameState level undefined
  resourceState <- newIORef $ LoadedResources [] []

  return ((gameState, inputState,resourceState), resourcesToLoad)
    where
      (w,h) = (50,50)

      -- ugly that this is here...
      resourcesToLoad =
          Resources {
              rShaderPrograms =
                  [ShaderProgramResource {
                     sprUniqueName =
                         "game2d",
                     sprVertShader =
                         ShaderInfo VertexShader $ FileSource ("assets" </> "shaders" </> "game2d.vert"),
                     sprFragShader =
                         ShaderInfo FragmentShader $ FileSource ("assets" </> "shaders" </> "game2d.frag")
                   },

                   ShaderProgramResource {
                     sprUniqueName =
                         "lol",
                     sprVertShader =
                         ShaderInfo VertexShader $ FileSource ("assets" </> "shaders" </> "lol.vert"),
                     sprFragShader =
                         ShaderInfo FragmentShader $ FileSource ("assets" </> "shaders" </> "lol.frag")
                   }

                  ],
              rTextures = []
            }
