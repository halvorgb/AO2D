module Model.Light where

import Graphics.Rendering.OpenGL
import qualified Linear as L

data Light =
  Sun {
    sPower :: GLfloat,
    sPosition :: L.V3 GLfloat,
    sColor :: L.V3 GLfloat
    }
  | NotImplementedLight
  deriving(Eq)