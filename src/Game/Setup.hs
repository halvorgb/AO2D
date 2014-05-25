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
import Model.ClearColor

import Model.Entity
import Model.Camera
import Model.Light

import Model.Colors

import qualified Linear as L



setupGame :: IO (State, Resources)
setupGame = do
  is <- newIORef $
        InputState [] (MouseInput 0 0)

  gs <- newIORef gameState

  rs <- newIORef $ LoadedResources M.empty M.empty M.empty

  let state = (gs, is, rs)

  return (state, resourcesToLoad)

gameState :: GameState
gameState = GameState entities camera lights clearColor
    where
      entities =
          [
           EntityInstance (L.V3 0 0 1) box2_ent (Just $ L.V3 0.2 0.1 0.25) Nothing,
           EntityInstance (L.V3 0 0 1.5) box2_ent (Just $ L.V3 0.3 0.1 0.25) $ Just red,
           EntityInstance (L.V3 0 0 2) box2_ent (Just $ L.V3 0.01 1.0 0.25) Nothing,
           EntityInstance (L.V3 1 0 0) box2_ent (Just $ L.V3 0.2 0.1 0.5) Nothing,
           EntityInstance (L.V3 1.5 0 0) box2_ent (Just $ L.V3 0.1 0.1 0.1) Nothing,
           EntityInstance (L.V3 2 0 0) box2_ent (Just $ L.V3 0.2 0.2 0.22) Nothing,

           EntityInstance (L.V3 0 1.5 0) lykt_ent Nothing Nothing,

           EntityInstance (L.V3 0 10 0) lightbox_ent Nothing Nothing,
           EntityInstance (L.V3 1 2 (-4)) lightbox_ent Nothing Nothing

          ]

      box2_ent =
        Entity "box2" (L.V3 1.0 1.0 1.0) white "standardShader" "box2Object" "box2Material"
      lightbox_ent =
        Entity "lightbox" (L.V3 0.05 0.05 0.05) yellow "lightSourceShader" "box2Object" "box2Material"
      lykt_ent =
        Entity "lykt" (L.V3 0.2 0.2 0.2) white "standardShader" "lyktObject" "placeholderMaterial"

      camera = Camera (L.V3 0 0 0) 0 0 90


      lights =
          [
           Light 8 (L.V3 0 10 0) white,
           Light 8 (L.V3 1 2 (-4)) white
          ]

      clearColor = defaultClearColor


resourcesToLoad :: Resources
resourcesToLoad =
    Resources {
  rShaderPrograms =
      [
       ShaderProgramResource {
         sprUniqueName = "standardShader",
         sprVertShader = "assets" </> "shaders" </> "standard.vert",
         sprFragShader = "assets" </> "shaders" </> "standard.frag"
       },
       ShaderProgramResource {
         sprUniqueName = "lightSourceShader",
         sprVertShader = "assets" </> "shaders" </> "standard.vert",
         sprFragShader = "assets" </> "shaders" </> "lightSource.frag"
       }

      ],

  rMaterials = [
   MaterialResource "box2Material" ("assets" </> "materials" </> "box2" </> "diffuse.png"),
   MaterialResource "placeholderMaterial" ("assets" </> "materials" </> "placeholder" </> "diffuse.png")
  ],

  rObjects  = [
   ObjectResource "box2Object" ("assets" </> "models" </> "box2.obj") ModelFormat'OBJ,
   ObjectResource "lyktObject" ("assets" </> "models" </> "LYKTSOTLP.obj") ModelFormat'OBJ

  ]
}
