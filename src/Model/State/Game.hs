module Model.State.Game where


import Model.Entity
import Model.Camera

import Model.Light
import Model.ClearColor

data GameState =
    GameState { gsEntities :: [EntityInstance],
                gsCamera :: Camera,
                gsLight :: Light,
                gsClearColor :: ClearColor
              } -- + more
