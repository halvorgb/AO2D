module Model.Entity where


import qualified Linear as L

data Entity =
    Entity { eName :: String,
             eScale :: Double,
             eShaderName :: String,
             eObjectName :: String
           } -- ++ more, texture??

data EntityInstance =
    EntityInstance { eiPosition :: L.V3 Double,
                     eiEntity :: Entity,
                     eiScaleOverride :: Maybe Double
                   } -- ++ more
