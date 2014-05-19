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



setupGame :: IO (State, Resources)
setupGame = do
  level <- generateLevel w h
  inputState    <- newIORef []
  gameState     <- newIORef $
                   GameState level [EntityInstance (L.V3 0 0 (-4)) ent $ Just 0.1,
                                    EntityInstance (L.V3 0 0 1) ent $ Just 1]
  resourceState <- newIORef $ LoadedResources M.empty [] M.empty undefined


  let state = (gameState, inputState, resourceState)

  return (state, resourcesToLoad)
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
                         "assets" </> "shaders" </> "cube.v.glsl",
                     sprFragShader =
                         "assets" </> "shaders" </> "cube.f.glsl"
                   }
                  ],
              rTextures = [],
              rObjects  = [ObjectGeometry "cubeGeometry" vertices elements colors]
            }



vertices :: [Vertex4 GLfloat]
vertices = [Vertex4 1.0      1.0    1.0  1.0,
            Vertex4 1.0      1.0  (-1.0) 1.0,
            Vertex4 1.0    (-1.0)   1.0  1.0,
            Vertex4 1.0    (-1.0) (-1.0) 1.0,
            Vertex4 (-1.0)   1.0    1.0  1.0,
            Vertex4 (-1.0)   1.0  (-1.0) 1.0,
            Vertex4 (-1.0) (-1.0)   1.0  1.0,
            Vertex4 (-1.0) (-1.0) (-1.0) 1.0
           ]

colors :: [Vertex4 GLfloat]
colors = vertices -- color space visualization

-- Vertices for each triangle in CCW order
elements :: [Vertex3 GLuint]
elements = [ Vertex3 2 1 0 -- right
           , Vertex3 1 2 3
           , Vertex3 0 1 4 -- top
           , Vertex3 4 1 5
           , Vertex3 4 5 6 -- left
           , Vertex3 7 6 5
           , Vertex3 2 6 3 -- bottom
           , Vertex3 6 3 7
           , Vertex3 0 4 2 -- front
           , Vertex3 2 4 6
           , Vertex3 5 1 7 -- back
           , Vertex3 7 1 3
           ]
