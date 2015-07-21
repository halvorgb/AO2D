module Model.Entity where

import           Model.Classes
import           Model.Collision
import           Model.Geometry
import           Model.Material
import           Model.Types

type UnloadedEntities = [EntityUnloaded]

data EntityUnloaded =
    EntityUnloaded { euUniqueName   :: String
                   , euRelativePos  :: Translation
                   , euRelativeRot  :: Rotation
                   , euScale        :: Scale
                   , euAmbOverride  :: Maybe GLfloat
                   , euGeometryName :: String
                   , euMaterialName :: String -- TODO, add more for specular/normal
                   }

data Entity =
    Entity { eRelativePos :: Translation
           , eRelativeRot :: Rotation
           , eBoundingBox :: BoundingBox
           , eScale       :: Scale
           , eAmbOverride :: Maybe GLfloat
           , eGeometry    :: Geometry
           , eMaterial    :: Material
           }


instance Transformable Entity where
  translationVector = eRelativePos
  rotationVector    = eRelativeRot
  scaleVector       = eScale

instance Collidable Entity where
  getBoundingBox e = transformCollidable base tm
    where base = eBoundingBox e
          tm   = mkTransMat e
