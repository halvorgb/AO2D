module Game.Setup(setupGame) where


import System.FilePath
import Data.IORef

import Graphics.Rendering.OpenGL

import Game.LevelGen.LevelGen

import Model.State
import Model.State.Game
import Model.State.Input()
import Model.State.Resources
import Model.ShaderProgram
import Model.Object


import Lib.LoadShaders (ShaderInfo(..), ShaderSource(..))


setupGame :: IO (State, Resources)
setupGame = do
  level <- generateLevel w h
  inputState    <- newIORef Nothing
  gameState     <- newIORef $ GameState level undefined
  resourceState <- newIORef $ LoadedResources [] [] []

  return ((gameState, inputState,resourceState), resourcesToLoad)
    where
      (w,h) = (50,50)

      -- ugly that this is here...
      resourcesToLoad =
          Resources {
              rShaderPrograms =
                  [ShaderProgramResource {
                     sprUniqueName =
                         "helloWorld",
                     sprVertShader =
                         ShaderInfo VertexShader $ FileSource ("assets" </> "shaders" </> "helloWorld.vert"),
                     sprFragShader =
                         ShaderInfo FragmentShader $ FileSource ("assets" </> "shaders" </> "helloWorld.frag")
                   }
                  ],
              rTextures = [],
              rObjects  = [ObjectGeometry "what" []]
            }
