module Game.Setup(setupGame) where


import System.FilePath
import Data.IORef
import qualified Data.Map as M

import Model.State
import Model.State.Game
import Model.State.Input
import Model.State.Resources
import Model.ShaderProgram
import Model.Object
import Model.Material

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
                        [EntityInstance (L.V3 0 0 1) tetra_ent $ Just 0.5,
                         EntityInstance (L.V3 0 0 (-4)) cube_ent $ Just 0.1,
                         EntityInstance (L.V3 0 0.2 0.1) cube_ent $ Just 0.25],
                    gsCamera =
                        Camera (L.V3 0 0 0) 0 0 90 }
  resourceState <- newIORef $ LoadedResources M.empty M.empty M.empty

  let state = (gameState, inputState, resourceState)

  return (state, resourcesToLoad)
    where
      cube_ent =
          Entity "cube" 1.0 "testTexShader" "boxObject" "boxMaterial"
      tetra_ent =
          Entity "tetra" 1.0 "testTexShader" "ballObject" "ballMaterial"

      -- ugly that this is here...
      resourcesToLoad =
          Resources {
              rShaderPrograms =
                  [ShaderProgramResource {
                     sprUniqueName =
                         "testTexShader",
                     sprVertShader =
                         "assets" </> "shaders" </> "testTex.vert",
                     sprFragShader =
                         "assets" </> "shaders" </> "testTex.frag"
                   }
                  ],
              rMaterials = [MaterialResource "boxMaterial" ("assets" </> "materials" </> "box" </> "diffuse.png"),
                            MaterialResource "ballMaterial" ("assets" </> "materials" </> "ball" </> "diffuse.png")],

              rObjects  = [ObjectResource "boxObject" ("assets" </> "models" </> "box.obj") ModelFormat'OBJ,
                           ObjectResource "ballObject" ("assets" </> "models" </> "ball.obj") ModelFormat'OBJ
                          ]
            }
