module Model.Collision where

import Data.List
import qualified Linear      as L
import           Model.Types

class Collidable a where
    getBoundingBox :: a -> BoundingBox

    rayIntersection :: a -> Bool
    rayIntersection bb = False

    withinDistance :: Translation -> GLfloat -> a -> Bool
    withinDistance position distane bb = False

    collidesWith :: Collidable b => a -> b -> Bool
    collidesWith bba bbb = False

data BoundingBox =
    BoundingBox { bbMin :: L.V3 GLfloat
                , bbMax :: L.V3 GLfloat
                }
    deriving (Show)


transformCollidable :: BoundingBox -> TransformationMatrix -> BoundingBox
transformCollidable (BoundingBox bbmax bbmin) tm = BoundingBox (transformBBVec bbmax) (transformBBVec bbmin)
  where transformBBVec :: L.V3 GLfloat -> L.V3 GLfloat
        transformBBVec v = L.normalizePoint $ tm L.!* (L.point v)

-- Combine several bounding boxes into one.
combineBBs :: [BoundingBox] -> BoundingBox
combineBBs [] = error "No boxes to combine..."
combineBBs (b:bs) = foldl' (\(BoundingBox (L.V3 maxx  maxy  maxz ) (L.V3 minx  miny  minz ))
                             (BoundingBox (L.V3 maxx' maxy' maxz') (L.V3 minx' miny' minz')) ->
                            (BoundingBox
                             (L.V3 (max maxx maxx') (max maxy maxy') (max maxz maxz'))
                             (L.V3 (min minx minx') (min miny miny') (min minz minz'))))
                    b bs

boundingBoxCorners :: BoundingBox -> [L.V3 GLfloat]
boundingBoxCorners (BoundingBox (L.V3 maxx maxy maxz) (L.V3 minx miny minz)) =
  [L.V3 x y z | x <- [minx, maxx], y <- [miny, maxy], z <- [minz, maxz]]
