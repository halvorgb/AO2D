module Game.Setup(setupGame) where


import System.FilePath
import Data.IORef
import qualified Data.Map as M

import Graphics.Rendering.OpenGL

import Model.State
import Model.State.Game
import Model.State.Input
import Model.State.Resources
import Model.ShaderProgram
import Model.Object
import Model.Entity
import Model.Camera

import qualified Linear as L



setupGame :: IO (State, Resources)
setupGame = do
  inputState    <- newIORef $
                   InputState [] (MouseInput 0 0)

  gameState     <- newIORef $
                   GameState {
                    gsEntities =
                        [EntityInstance (L.V3 0 0 0) tetra_ent $ Just 0.5,
                         EntityInstance (L.V3 0 0 (-4)) cube_ent $ Just 0.1,
                         EntityInstance (L.V3 0 0.2 0.1) cube_ent $ Just 0.25],
                    gsCamera =
                        Camera (L.V3 0 0 0) 0 0 100 }
  resourceState <- newIORef $ LoadedResources M.empty [] M.empty


  let state = (gameState, inputState, resourceState)

  return (state, resourcesToLoad)
    where
      cube_ent =
          Entity "cube" 1.0 "cubeShader" "cubeGeometry"
      tetra_ent =
          Entity "tetra" 1.0 "cubeShader" "tetraGeometry"

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
              rObjects  = [ObjectGeometry "tetraGeometry" tetra_vertices tetra_elements tetra_colors,
                           ObjectGeometry "cubeGeometry" cube_vertices cube_elements cube_colors]
            }



cube_vertices :: [Vertex4 GLfloat]
cube_vertices = [Vertex4 1.0      1.0    1.0  1.0,
                 Vertex4 1.0      1.0  (-1.0) 1.0,
                 Vertex4 1.0    (-1.0)   1.0  1.0,
                 Vertex4 1.0    (-1.0) (-1.0) 1.0,
                 Vertex4 (-1.0)   1.0    1.0  1.0,
                 Vertex4 (-1.0)   1.0  (-1.0) 1.0,
                 Vertex4 (-1.0) (-1.0)   1.0  1.0,
                 Vertex4 (-1.0) (-1.0) (-1.0) 1.0
           ]

cube_colors :: [Vertex4 GLfloat]
cube_colors = cube_vertices -- color space visualization

-- Vertices for each triangle in CCW order
cube_elements :: [Vertex3 GLuint]
cube_elements = [ Vertex3 2 1 0 -- right
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


tetra_vertices :: [Vertex4 GLfloat]
tetra_vertices =[Vertex4 1.0      1.0    1.0  1.0,
                 Vertex4 1.0    (-1.0) (-1.0) 1.0,
                 Vertex4 (-1.0)   1.0  (-1.0) 1.0,
                 Vertex4 (-1.0) (-1.0)   1.0  1.0]

tetra_colors :: [Vertex4 GLfloat]
tetra_colors = tetra_vertices

tetra_elements :: [Vertex3 GLuint]
tetra_elements = [Vertex3 0 1 2,
                  Vertex3 1 2 3,
                  Vertex3 0 1 3,
                  Vertex3 0 3 2]
