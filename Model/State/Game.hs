module Model.State.Game where


import Model.Entity
import Model.Camera


data GameState =
    GameState { gsEntities :: [EntityInstance],
                gsCamera :: Camera
              } -- + more
