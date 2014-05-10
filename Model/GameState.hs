module Model.GameState where


import Model.Level
import Model.Entity

data GameState =
    GameState { gsLevel :: Level,
                gsEntities :: EntityMap -- + more
              }
    deriving (Show, Eq)
