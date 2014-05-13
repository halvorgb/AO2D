module Game.Setup(setupGame) where


import Control.Applicative
import System.FilePath
import Data.IORef
import qualified Data.Map as M

import Graphics.Rendering.OpenGL

import Game.LevelGen.LevelGen
import Game.Update

import Model.State
import Model.State.Game
import Model.State.Input()
import Model.State.Resources
import Model.ShaderProgram
import Model.Object
import Model.Entity

import qualified Linear as L


import Lib.LoadShaders (ShaderInfo(..), ShaderSource(..))


setupGame :: IO (State, Resources, (Double -> IO ([Vertex3 GLdouble], [Color3 GLdouble])))
setupGame = do
  level <- generateLevel w h
  inputState    <- newIORef Nothing
  gameState     <- newIORef $
                   GameState level [EntityInstance (L.V3 0 0 0) ent Nothing]
  resourceState <- newIORef $ LoadedResources M.empty [] M.empty


  let state = (gameState, inputState, resourceState)
      updateFunc = updateGame state

  return (state, resourcesToLoad, updateFunc)
    where
      (w,h) = (50,50)

      ent =
          Entity "cube" 1.0 "cubeShader" "cubeGeometry"


      -- ugly that this is here...
      resourcesToLoad =
          Resources {
              rShaderPrograms =
                  [ShaderProgramResource {
                     sprUniqueName =
                         "cubeShader",
                     sprVertShader =
                         ("assets" </> "shaders" </> "cube.v.glsl"),
                     sprFragShader =
                         ("assets" </> "shaders" </> "cube.f.glsl")
                   }
                  ],
              rTextures = [],
              rObjects  = [ObjectGeometry "cubeGeometry" vertices elements colors]
            }



vertices :: [L.V3 Float]
vertices = L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]

colors :: [L.V3 Float]
colors = vertices -- color space visualization

-- Vertices for each triangle in CCW order
elements :: [L.V3 GLuint]
elements = [ L.V3 2 1 0 -- right
           , L.V3 1 2 3
           , L.V3 0 1 4 -- top
           , L.V3 4 1 5
           , L.V3 4 5 6 -- left
           , L.V3 7 6 5
           , L.V3 2 6 3 -- bottom
           , L.V3 6 3 7
           , L.V3 0 4 2 -- front
           , L.V3 2 4 6
           , L.V3 5 1 7 -- back
           , L.V3 7 1 3
           ]
