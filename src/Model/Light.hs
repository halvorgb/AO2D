module Model.Light where

import           Model.Object
import           Model.Types

data PointLight =
    PointLight { plPosition   :: Translation
               , plStrength   :: GLfloat
               , plRadius     :: GLfloat
               , plColor      :: ColorRGB
               , plrelativeTo :: Maybe Object
               }
