module Model.Object where

import Model.Classes
import Model.Types
import Model.Collision
import Model.Entity

type UnloadedObjects = [Object'Unloaded]

data Object'Unloaded =
    Object'Unloaded {
      ouPosition :: Translation,
      ouRotation :: Rotation,
      ouScale :: Scale,

      ouEntityNames :: [String]
    }

data Object =
    Object { oPosition  :: Translation,
             oRotation  :: Rotation,
             oScale     :: Scale,

             oBBT       :: BoundingBoxTree,

             oEntities  :: [Entity]
           }

instance Transformable Object where
    translationVector = oPosition
    rotationVector    = oRotation
    scaleVector       = oScale


instance Collidable Object where
    getBBT = oBBT
