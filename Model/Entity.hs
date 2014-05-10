module Model.Entity where


import Graphics.Rendering.OpenGL

import Model.Common as C
import qualified Data.Map as M

type Entities = [EntityInstance]


data Entity =
    Entity { eName :: String,
             eModel :: Model,
             eShader :: Shader,
             eScale :: Double
           } -- ++ more
    deriving (Show, Eq)

data EntityInstance =
    EntityInstance { eiPosition :: C.Position,
                     eiEntity :: Entity,
                     eiScaleOverride :: Maybe Double
                   } -- ++ more
    deriving (Show, Eq)
