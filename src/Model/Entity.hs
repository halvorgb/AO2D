module Model.Entity where


import qualified Linear as L
import qualified Graphics.GLUtil as GLUtil

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

      euShaderName   :: String,
      euGeometryName :: String,
      euMaterialName :: String -- TODO, add more for specular/normal
    }

data Entity =
    Entity {
      eRelativePos :: Translation,
      eRelativeRot :: Rotation,
      eScale       :: Scale,

      eShader      :: GLUtil.ShaderProgram,
      eGeometry    :: Geometry,
      eMaterial    :: Material
    }


instance Transformable Entity where
    translationVector = eRelativePos
    rotationVector    = eRelativeRot
    scaleVector       = eScale
