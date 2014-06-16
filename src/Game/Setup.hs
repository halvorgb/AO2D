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
import Model.ShaderPrograms

import qualified Linear as L



setupGame :: IO InitialState
setupGame = do
  w <- world
  return (w, resources, unloadedObjects, unloadedEntities, unloadedShaderPrograms)

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
              gsAmbiance = 0.1,
              gsShaderPrograms = undefined -- overwritten later...
            }
    where
      camera = Camera (L.V3 0 0 0) 0 0 (pi/2)


      lights = [PointLight lightPos 4 (L.V3 1 1 1) Nothing]

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
         srUniqueName   = "depth",
         srVertShaderFP = "assets" </> "shaders" </> "depth.vert",
         srGeomShaderFP = Nothing,
         srFragShaderFP = Just $ "assets" </> "shaders" </> "depth.frag"
       },
       ShaderResource {
         srUniqueName   = "shadowVol",
         srVertShaderFP = "assets" </> "shaders" </> "shadowVolume.vert",
         srGeomShaderFP = Just $ "assets" </> "shaders" </> "shadowVolume.geom",
         srFragShaderFP = Just $ "assets" </> "shaders" </> "shadowVolume.frag"
       },
       ShaderResource {
         srUniqueName   = "light",
         srVertShaderFP = "assets" </> "shaders" </> "lighting.vert",
         srGeomShaderFP = Nothing,
         srFragShaderFP = Just $ "assets" </> "shaders" </> "lighting.frag"
       }]
      materialResources = [
       MaterialResource {
         mrUniqueName =  "placeholder",
         mrDiffuseFP  = "assets" </> "materials" </> "placeholder" </> "diffuse.png",
         mrSpecularFP = undefined,
         mrNormalFP = undefined},
       MaterialResource {
         mrUniqueName =  "white",
         mrDiffuseFP  = "assets" </> "materials" </> "white" </> "diffuse.png",
         mrSpecularFP = undefined,
         mrNormalFP = undefined}]


unloadedObjects :: UnloadedObjects
unloadedObjects = [
 Object'Unloaded {
   ouPosition = L.V3(-0.7) (-0.5) 1.8,
   ouRotation = L.V3 0 0 0,
   ouScale = L.V3 0.25 0.25 0.25,

   ouEntityNames = ["box2"]
 },
 Object'Unloaded {
   ouPosition = L.V3 0 0 2,
   ouRotation = L.V3 0 0 0,
   ouScale = L.V3 0.25 0.25 0.25,

   ouEntityNames = ["box2"]
 },
  Object'Unloaded {
   ouPosition = L.V3 3 1 0,
   ouRotation = L.V3 0 0 0,
   ouScale = L.V3 0.25 0.25 0.25,

   ouEntityNames = ["lykt"]
 },
 Object'Unloaded {
   ouPosition = L.V3 0 (-1) 0,
   ouRotation = L.V3 0 0 0,
   ouScale = L.V3 3 0.05 3,

   ouEntityNames = ["box2"]
 }]


unloadedEntities :: UnloadedEntities
unloadedEntities = [
 Entity'Unloaded {
   euUniqueName  = "box2",
   euRelativePos = L.V3 0 0 0,
   euRelativeRot = L.V3 0 0 0,
   euScale       = L.V3 1 1 1,

   euAmbOverride = Nothing,

   euGeometryName = "box2",
   euMaterialName = "placeholder"
 },
 Entity'Unloaded {
   euUniqueName  = "lykt",
   euRelativePos = L.V3 0 0 0,
   euRelativeRot = L.V3 0 0 0,
   euScale       = L.V3 1 1 1,

   euAmbOverride = Nothing,

   euGeometryName = "lykt",
   euMaterialName = "placeholder"
 },
 Entity'Unloaded {
   euUniqueName  = "light_box",
   euRelativePos = L.V3 0 0 0,
   euRelativeRot = L.V3 0 0 0,
   euScale       = L.V3 1 1 1,

   euAmbOverride = Just 1, -- fullbright

   euGeometryName = "box2",
   euMaterialName = "white"
 }]

unloadedShaderPrograms :: ShaderPrograms'Unloaded
unloadedShaderPrograms =
    ShaderPrograms'Unloaded {
  spDepthName =  "depth",
  spShadowVolName  =  "shadowVol",
  spLightName = "light"
}




lightPos = L.V3 1 2 3
