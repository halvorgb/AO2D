module Model.State.Game where


import Model.Level
import Model.Entity

data GameState =
    GameState { gsLevel :: Level,
                gsEntities :: [EntityInstance]
              } -- + more
