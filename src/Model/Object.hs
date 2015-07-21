module Model.Object where

import           Model.Classes
import           Model.Collision
import           Model.Entity
import           Model.Types

type UnloadedObjects = [ObjectUnloaded]

data ObjectUnloaded =
    ObjectUnloaded { ouPosition    :: Translation
                   , ouRotation    :: Rotation
                   , ouScale       :: Scale
                   , ouEntityNames :: [String]
                   }

data Object =
    Object { oPosition :: Translation
           , oRotation :: Rotation
           , oScale    :: Scale
           , oEntities :: [Entity]
           }

instance Transformable Object where
  translationVector = oPosition
  rotationVector    = oRotation
  scaleVector       = oScale


instance Collidable Object where
  getBoundingBox o = transformCollidable base tm
    where base = combineBBs $ map getBoundingBox $ oEntities o
          tm = mkTransMat o
