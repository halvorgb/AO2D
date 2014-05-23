module Model.Light where

import Graphics.Rendering.OpenGL.Raw.Types
import qualified Linear as L

data Light =
  Light {
    lStrength :: GLfloat,
    lPosition :: L.V3 GLfloat,
    lColor :: L.V3 GLfloat
    }
  deriving(Eq)

getVectors :: Light -> (L.V3 GLfloat, L.V3 GLfloat, L.V3 GLfloat)
getVectors l = (sVec, pVec, lVec)
  where s    = lStrength l
        sVec = L.V3 s 0 0
        pVec = lPosition l
        lVec = lColor l
