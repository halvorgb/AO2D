module Model.Entity where


import Model.Common
import qualified Data.Map as M

type EntityMap = M.Map Position Entity


data Entity =
    Entity { eName :: String
           } -- ++ more
    deriving (Show, Eq)

data EntityInstance =
    EntityInstance { eiPosition :: Position,
                     eiEntity :: Entity
                   } -- ++ more
    deriving (Show, Eq)
