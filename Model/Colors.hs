module Model.Colors (white, red, blue, green, yellow, purple, teal) where

import Graphics.Rendering.OpenGL
import qualified Linear as L

mkColor :: GLfloat -> GLfloat -> GLfloat -> L.V3 GLfloat
mkColor = L.V3

white, red, green, blue, yellow, purple, teal :: L.V3 GLfloat
white  = mkColor 1 1 1
red    = mkColor 1 0 0
blue   = mkColor 0 0 1
green  = mkColor 0 1 0
yellow = mkColor 1 1 0
purple = mkColor 1 0 1
teal   = mkColor 0 1 1
