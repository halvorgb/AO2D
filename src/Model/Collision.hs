module Model.Collision where


import Model.Types

class Collidable a where
    getBBT :: a -> BoundingBoxTree

    rayIntersection :: a -> Bool
    rayIntersection bbt = False

    withinDistance  :: Translation -> GLfloat -> a -> Bool
    withinDistance  position distane bbt = False

    collidesWith :: Collidable b => a -> b -> Bool
    collidesWith bbta bbtb = False
data BoundingBox =
    BoundingBox {
      bbMaxX :: GLfloat,
      bbMinX :: GLfloat,
      bbMaxY :: GLfloat,
      bbMinY :: GLfloat,
      bbMaxZ :: GLfloat,
      bbMinZ :: GLfloat } -- corners instead?


type BBT = BoundingBoxTree
data BoundingBoxTree =
    BoundingBoxTree BoundingBox BoundingBoxTree BoundingBoxTree
  | BBTLeaf
