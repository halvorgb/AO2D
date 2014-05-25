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
