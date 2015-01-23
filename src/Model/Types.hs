module Model.Types( TransformationMatrix
                  , TransformationVector
                  , Translation
                  , Rotation
                  , Scale
                  , Color'RGB
                  , GLfloat
                  , GLint
                  , GLuint
                  ) where

import           Graphics.Rendering.OpenGL
import qualified Linear                    as L

type TransformationMatrix = L.M44 GLfloat
type TransformationVector = L.V3 GLfloat

type Translation = L.V3 GLfloat
type Scale       = L.V3 GLfloat
type Rotation    = L.V3 GLfloat

type Color'RGB   = L.V3 GLfloat
