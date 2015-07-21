module Engine.Graphics.Culling(frustumCull, radiusCull) where

import           Graphics.Rendering.OpenGL
import qualified Linear                    as L
import           Model.Object
import           Model.Types
import           Model.Collision

frustumCull :: [Object] -> TransformationMatrix -> GLfloat -> GLfloat-> [Object]
frustumCull os tm nclip fclip = os

radiusCull :: [Object] -> Translation -> GLfloat -> [Object]
radiusCull os center radius =
  filter (\o -> (minDist center $ getBoundingBox o) < radius) os

minDist :: Translation -> BoundingBox -> GLfloat
minDist p bb = minimum $ map (L.distance p) $ boundingBoxCorners bb
