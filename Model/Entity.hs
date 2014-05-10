module Model.Entity where


import Graphics.Rendering.OpenGL

import Model.Common as C
type Entities = [EntityInstance]


data Entity =
    Entity { eName :: String,
             eScale :: Double
           } -- ++ more
    deriving (Show, Eq)

data EntityInstance =
    EntityInstance { eiPosition :: C.Position,
                     eiEntity :: Entity,
                     eiScaleOverride :: Maybe Double
                   } -- ++ more
    deriving (Show, Eq)
