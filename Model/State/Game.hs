module Model.State.Game where


import Model.Entity
import Model.Camera

import Model.Light

data GameState =
    GameState { gsEntities :: [EntityInstance],
                gsCamera :: Camera,
                gsSun :: Light
              } -- + more
