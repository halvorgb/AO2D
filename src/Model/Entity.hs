module Model.Entity where

import Model.Material
import Model.Geometry
import Model.Types
import Model.Classes

type UnloadedEntities = [Entity'Unloaded]

data Entity'Unloaded =
    Entity'Unloaded {
      euUniqueName :: String,

      euRelativePos :: Translation,
      euRelativeRot :: Rotation,
      euScale       :: Scale,

      euAmbOverride :: Maybe GLfloat,

      euGeometryName :: String,
      euMaterialName :: String -- TODO, add more for specular/normal
    }

data Entity =
    Entity {
      eRelativePos :: Translation,
      eRelativeRot :: Rotation,
      eScale       :: Scale,

      eAmbOverride :: Maybe GLfloat,

      eGeometry    :: Geometry,
      eMaterial    :: Material
    }


instance Transformable Entity where
    translationVector = eRelativePos
    rotationVector    = eRelativeRot
    scaleVector       = eScale
