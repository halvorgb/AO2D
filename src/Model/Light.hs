module Model.Light where

import Model.Types
import Model.Object

data PointLight =
    PointLight {
      plPosition   :: Translation,
      plStrength   :: GLfloat,
      plColor      :: Color'RGB,
      plrelativeTo :: Maybe Object
    }
