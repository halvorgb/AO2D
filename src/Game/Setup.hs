module Game.Setup(setupGame) where

import           Data.IORef
import qualified Linear               as L
import           Model.Camera
import           Model.ClearColor
import           Model.Entity
import           Model.GameState
import           Model.InputState
import           Model.Light
import           Model.Object
import           Model.Resources
import           Model.ShaderPrograms
import           Model.Types
import           Model.World
import           System.FilePath

setupGame :: IO InitialState
setupGame = do
  w <- world
  return (w, resources, unloadedObjects, unloadedEntities, unloadedShaderPrograms)

world :: IO World
world = do
  isIO <- newIORef inputState
  return (gameState, isIO)

inputState :: InputState
inputState =
  InputState { isKeyboardInput = []
             , isMouseInput = MouseInput 0 0
             }

gameState :: GameState
gameState =
  GameState { gsCamera = camera
            , gsObjects = [] -- to be loaded
            , gsLights = lights
            , gsClearColor = clearColor
            , gsAmbiance = 0.1
            , gsShaderPrograms = undefined -- overwritten later...
            }
  where camera = Camera (L.V3 0 0 0) 0 0 (pi/2)

        lights = map snd lamps

        clearColor = defaultClearColor

resources :: Resources
resources =
  Resources { rGeometryRs = geometryResources
            , rShaderRs   = shaderResources
            , rMaterialRs = materialResources
            }

    where geometryResources =
            [ GeometryResource { grUniqueName  = "box2"
                               , grModelFormat = ModelFormatOBJ
                               , grModelFP     = "assets" </> "models" </> "box2.obj"
                               }
            , GeometryResource { grUniqueName  = "lykt"
                               , grModelFormat = ModelFormatOBJ
                               , grModelFP     = "assets" </> "models" </> "LYKTSOTLP.obj"
                               }
            ]

          shaderResources =
            [ ShaderResource { srUniqueName   = "depth"
                             , srVertShaderFP = "assets" </> "shaders" </> "depth.vert"
                             , srGeomShaderFP = Nothing
                             , srFragShaderFP = Just $ "assets" </> "shaders" </> "depth.frag"
                             }
            , ShaderResource { srUniqueName   = "shadowVol"
                             , srVertShaderFP = "assets" </> "shaders" </> "shadowVolume.vert"
                             , srGeomShaderFP = Just $ "assets" </> "shaders" </> "shadowVolume.geom"
                             , srFragShaderFP = Just $ "assets" </> "shaders" </> "shadowVolume.frag"
                             }
            , ShaderResource { srUniqueName   = "light"
                             , srVertShaderFP = "assets" </> "shaders" </> "lighting.vert"
                             , srGeomShaderFP = Nothing
                             , srFragShaderFP = Just $ "assets" </> "shaders" </> "lighting.frag"
                             }
            ]

          materialResources =
            [ MaterialResource { mrUniqueName =  "placeholder"
                               , mrDiffuseFP  = "assets" </> "materials" </> "placeholder" </> "diffuse.png"
                               , mrSpecularFP = undefined
                               , mrNormalFP   = undefined
                               }
            , MaterialResource { mrUniqueName =  "white"
                               , mrDiffuseFP  = "assets" </> "materials" </> "white" </> "diffuse.png"
                               , mrSpecularFP = undefined
                               , mrNormalFP   = undefined
                               }
            ]


unloadedObjects :: UnloadedObjects
unloadedObjects =
  map fst lamps ++
  boxes ++
  [ ObjectUnloaded {  ouPosition    = L.V3 3 1 0
                   , ouRotation    = L.V3 0 0 0
                   , ouScale       = L.V3 0.25 0.25 0.25
                   , ouEntityNames = ["lykt"]
                   }
  ]


unloadedEntities :: UnloadedEntities
unloadedEntities =
  [ EntityUnloaded { euUniqueName  = "box2"
                   , euRelativePos = L.V3 0 0 0
                   , euRelativeRot = L.V3 0 0 0
                   , euScale       = L.V3 1 1 1
                   , euAmbOverride = Nothing
                   , euGeometryName = "box2"
                   , euMaterialName = "placeholder"
                   }
  , EntityUnloaded { euUniqueName  = "lykt"
                   , euRelativePos = L.V3 0 0 0
                   , euRelativeRot = L.V3 0 0 0
                   , euScale       = L.V3 1 1 1
                   , euAmbOverride = Just 1
                   , euGeometryName = "lykt"
                   , euMaterialName = "placeholder"
                   }
  , EntityUnloaded { euUniqueName  = "light_box"
                   , euRelativePos = L.V3 0 0 0
                   , euRelativeRot = L.V3 0 0 0
                   , euScale       = L.V3 1 1 1
                   , euAmbOverride = Just 1 -- fullbright
                   , euGeometryName = "box2"
                   , euMaterialName = "white"
                   }
  ]

unloadedShaderPrograms :: ShaderProgramsUnloaded
unloadedShaderPrograms =
  ShaderProgramsUnloaded { spShadowVolName = "shadowVol"
                         , spLightName     = "light"
                         , spDepthName     = "depth"
                         }

lamps :: [(ObjectUnloaded, PointLight)]
lamps = [ createLamp (L.V3 (-1) 3 (-1)) (L.V3 0 0 0) (L.V3 0.01 0.01 0.01) "light_box" (L.V3 0.8 0.8 0.8)
--        , createLamp (L.V3 2    4   4)  (L.V3 0 0 0) (L.V3 0.01 0.01 0.01) "light_box" (L.V3 0.8 0.8 0.8)
        ]
createLamp :: Translation -> Rotation -> Scale -> String -> ColorRGB -> (ObjectUnloaded, PointLight)
createLamp pos rot scale entName clr =
  ( ObjectUnloaded pos rot scale [entName],
    PointLight pos 0 clr Nothing
  )


boxes :: [ObjectUnloaded]
boxes = [ createBox (L.V3(-0.7) (-0.5) 1.8) zeroRot quarterScale "box2"
        , createBox (L.V3 0 0 2)            zeroRot quarterScale "box2"
        , createBox (L.V3 0 1 2)            zeroRot quarterScale "box2"
        , createBox (L.V3 0 2 2)            zeroRot quarterScale "box2"
        , createBox (L.V3 0 2 3)            zeroRot quarterScale "box2"
        , createBox (L.V3 0 (-1) 0)         zeroRot (L.V3 10 0.01 10) "box2"
        ]
  where zeroRot      = L.V3 0 0 0
        quarterScale = L.V3 0.25 0.25 0.25
createBox :: Translation -> Rotation -> Scale -> String -> ObjectUnloaded
createBox pos rot scale entName =
  ObjectUnloaded pos rot scale [entName]
