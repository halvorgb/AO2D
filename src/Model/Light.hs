module Model.Light where

import           Model.Object
import           Model.Types

data PointLight =
    PointLight { plPosition   :: Translation
               , plStrength   :: GLfloat
               , plColor      :: Color'RGB
               , plrelativeTo :: Maybe Object
               }
