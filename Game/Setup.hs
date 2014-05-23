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

  gameState     <- newIORef
                   GameState {
                    gsEntities =
                        [
                         EntityInstance (L.V3 0 0 1) box2_ent (Just $ L.V3 0.2 0.1 0.25) (Just $ L.V4 0.3 0.0 0.1 1.0),
                         EntityInstance (L.V3 0 0 1.5) box2_ent (Just $ L.V3 0.3 0.1 0.25) (Just $ L.V4 0.0 0.1 0.5 1.0),
                         EntityInstance (L.V3 0 0 2) box2_ent (Just $ L.V3 0.01 1.0 0.25) (Just $ L.V4 0.1 0.5 0.0 1.0),
                         EntityInstance (L.V3 1 0 0) box2_ent (Just $ L.V3 0.2 0.1 0.5) (Just $ L.V4 0.0 1.0 0.0 1.0),
                         EntityInstance (L.V3 1.5 0 0) box2_ent (Just $ L.V3 0.1 0.1 0.1) (Just $ L.V4 0.0 0.0 1.0 1.0),
                         EntityInstance (L.V3 2 0 0) box2_ent (Just $ L.V3 0.2 0.2 0.22) (Just $ L.V4 1.0 0.0 0.0 1.0)
                        ],
                    gsCamera =
                        Camera (L.V3 0 0 0) 0 0 90 }
  resourceState <- newIORef $ LoadedResources M.empty M.empty M.empty

  let state = (gameState, inputState, resourceState)

  return (state, resourcesToLoad)
    where
      box2_ent =
          Entity "box2" (L.V3 1.0 1.0 1.0) (L.V4 1.0 1.0 1.0 1.0) "testTexShader" "box2Object" "box2Material"

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
              rMaterials = [MaterialResource "box2Material" ("assets" </> "materials" </> "box2" </> "diffuse.png")
                           ],

              rObjects  = [ObjectResource "box2Object" ("assets" </> "models" </> "box2.obj") ModelFormat'OBJ
                          ]
            }
