module Game.Setup(setupGame) where

import           Data.IORef
import qualified Data.Map              as M
import qualified Linear                as L
import           Mish.Config
import           Mish.HexagonalGrid
import           Mish.MissionGenerator
import           Model.Camera
import           Model.ClearColor
import           Model.Entity
import           Model.GameState
import           Model.InputState
import           Model.Light
import           Model.Object
import           Model.Types
import           Model.World
import           System.Random

setupGame :: IO InitialState
setupGame = do
  w <- world

  mishSeed <- newStdGen
  let mish = generateMission defaultConfig mishSeed
  return (w, unloadedObjects mish, unloadedEntities)

missionConfig =
  Config { radius                = 16
         , roomAttempts          = 100
         , minRoomRadius         = 1
         , maxRoomRadius         = 3
         , doubleConnectorChance = 0.1
         }


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

unloadedObjects :: HexagonalMission -> UnloadedObjects
unloadedObjects hm =
  map fst lamps ++
  hexes
  where ascs = M.assocs $ getInternalMap hm
        hexes = map createHex ascs


unloadedEntities :: UnloadedEntities
unloadedEntities =
  [ EntityUnloaded { euUniqueName   = "box2"
                   , euRelativePos  = L.V3 0 0 0
                   , euRelativeRot  = L.V3 0 0 0
                   , euScale        = L.V3 1 1 1
                   , euAmbOverride  = Nothing
                   , euGeometryName = "box2"
                   , euMaterialName = "placeholder"
                   }
  , EntityUnloaded { euUniqueName   = "hexagon_white"
                   , euRelativePos  = L.V3 0 0 0
                   , euRelativeRot  = L.V3 0 0 0
                   , euScale        = L.V3 1 1 1
                   , euAmbOverride  = Nothing
                   , euGeometryName = "hexagon"
                   , euMaterialName = "white"
                   }
  , EntityUnloaded { euUniqueName   = "hexagon_gray"
                   , euRelativePos  = L.V3 0 0 0
                   , euRelativeRot  = L.V3 0 0 0
                   , euScale        = L.V3 1 1 1
                   , euAmbOverride  = Nothing
                   , euGeometryName = "hexagon"
                   , euMaterialName = "gray"
                   }
  , EntityUnloaded { euUniqueName   = "light_box"
                   , euRelativePos  = L.V3 0 0 0
                   , euRelativeRot  = L.V3 0 0 0
                   , euScale        = L.V3 1 1 1
                   , euAmbOverride  = Just 1 -- fullbright
                   , euGeometryName = "box2"
                   , euMaterialName = "white"
                   }
  ]

lamps :: [(ObjectUnloaded, PointLight)]
lamps = [ createLamp (L.V3 (-1) 3 (-1)) (L.V3 0 0 0) (L.V3 0.01 0.01 0.01) "light_box" (L.V3 0.8 0.8 0.8)
        , createLamp (L.V3 2    4   4)  (L.V3 0 0 0) (L.V3 0.01 0.01 0.01) "light_box" (L.V3 0.8 0.8 0.8)
        ]
createLamp :: Translation -> Rotation -> Scale -> String -> ColorRGB -> (ObjectUnloaded, PointLight)
createLamp pos rot scale entName clr =
  ( ObjectUnloaded pos rot scale [entName],
    PointLight pos 100 5 clr Nothing
  )

createHex :: (AxialCoordinate, Tile) -> ObjectUnloaded
createHex ((q,r),t) = ObjectUnloaded pos rot scale [entName]
  where
    pos = L.V3 (hexSize * (sqrt 3) * fir + fiq/2) 0 (hexSize * 1.5 * fiq)
    rot = L.V3 0 0 0
    (entName, scale) = if t == Wall
                       then ("hexagon_gray", L.V3 (1.0-spacing) 2.0 (1.0-spacing))
                       else ("hexagon_white", L.V3 (1.0-spacing) 1.0 (1.0-spacing))


    fiq = fi q
    fir = fi r


hexSize, spacing :: GLfloat
spacing = 0.1
hexSize = 0.5

fi = fromIntegral
