module Model.Entity where

import Graphics.Rendering.OpenGL

import qualified Linear as L

data Entity =
    Entity { eName :: String,
             eScale :: L.V3 GLfloat,
             eColor :: L.V4 GLfloat,
             eShaderName :: String,
             eObjectName :: String,
             eMaterialName :: String
           } -- ++ more, texture??

data EntityInstance =
    EntityInstance { eiPosition :: L.V3 GLfloat,
                     eiEntity :: Entity,
                     eiScaleOverride :: Maybe (L.V3 GLfloat),
                     eiColorOverride :: Maybe (L.V4 GLfloat)
                   } -- ++ more
