module Game.Setup(setupGame) where


import System.FilePath
import Data.IORef

import Model.GameState
import Model.InputState
import Model.Resources

import Model.Object
import Model.ClearColor

import Model.Entity
import Model.Camera
import Model.Light
import Model.World


import qualified Linear as L



setupGame :: IO InitialState
setupGame = do
  w <- world
  return (w, resources, unloadedObjects, unloadedEntities)

world :: IO World
world = do
  isIO <- newIORef inputState
  return (gameState, isIO)

inputState :: InputState
inputState = InputState {
               isKeyboardInput = [],
               isMouseInput = MouseInput 0 0
             }

gameState :: GameState
gameState = GameState {
              gsCamera = camera,
              gsObjects = [], -- to be loaded.
              gsLights = lights,
              gsClearColor = clearColor,
              gsAmbiance = L.V3 0.1 0.1 0.1
            }
    where
      camera = Camera (L.V3 0 0 0) 0 0 (pi/2)


      lights = [PointLight (L.V3 0 0 2) 10 (L.V3 1 1 1) Nothing]

      clearColor = defaultClearColor

resources :: Resources
resources = Resources {
              rGeometryRs = geometryResources,
              rShaderRs   = shaderResources,
              rMaterialRs = materialResources
            }
    where
      geometryResources = [
       GeometryResource {
         grUniqueName  = "box2",
         grModelFormat = ModelFormat'OBJ,
         grModelFP     = "assets" </> "models" </> "box2.obj"
       },
       GeometryResource {
         grUniqueName  = "lykt",
         grModelFormat = ModelFormat'OBJ,
         grModelFP     = "assets" </> "models" </> "LYKTSOTLP.obj"
       }]

      shaderResources = [
       ShaderResource {
         srUniqueName   = "standard",
         srVertShaderFP = "assets" </> "shaders" </> "standard.vert",
         srGeomShaderFP = "assets" </> "shaders" </> "standard.geom",
         srFragShaderFP = "assets" </> "shaders" </> "standard.frag"
       }]
      materialResources = [
       MaterialResource {
         mrUniqueName =  "placeholderDiffuse",
         mrDiffuseFP  = "assets" </> "materials" </> "placeholder" </> "diffuse.png",
         mrSpecularFP = undefined,
         mrNormalFP = undefined}]


unloadedObjects :: UnloadedObjects
unloadedObjects = [
 Object'Unloaded {
   ouPosition = L.V3 0 0 2,
   ouRotation = L.V3 0 0 0,
   ouScale = L.V3 0.25 0.25 0.25,

   ouEntityNames = ["box2"]
 },
  Object'Unloaded {
   ouPosition = L.V3 0 0 0,
   ouRotation = L.V3 0 0 0,
   ouScale = L.V3 0.25 0.25 0.25,

   ouEntityNames = ["lykt"]
 }]

unloadedEntities :: UnloadedEntities
unloadedEntities = [
 Entity'Unloaded {
   euUniqueName  = "box2",
   euRelativePos = L.V3 0 0 0,
   euRelativeRot = L.V3 0 0 0,
   euScale       = L.V3 1 1 1,

   euShaderName  = "standard",
   euGeometryName = "box2",
   euMaterialName = "placeholderDiffuse"
 },
 Entity'Unloaded {
   euUniqueName  = "lykt",
   euRelativePos = L.V3 0 0 0,
   euRelativeRot = L.V3 0 0 0,
   euScale       = L.V3 1 1 1,

   euShaderName  = "standard",
   euGeometryName = "lykt",
   euMaterialName = "placeholderDiffuse"
 }]
