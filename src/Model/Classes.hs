module Model.Classes where

import Model.Types

import qualified Linear as L

class Transformable a where
    translationVector :: a -> TransformationVector
    rotationVector    :: a -> TransformationVector
    scaleVector       :: a -> TransformationVector
    mkTransMat        :: a -> TransformationMatrix

    mkTransMat a = fmap (fmap realToFrac) transMat
        where (L.V3 tx ty tz) = translationVector a
              --(L.V3 rx ry rz) = rotationVector a
              (L.V3 sx sy sz) = scaleVector a

              transMat = L.V4
                   (L.V4 sx 0  0  tx)
                   (L.V4 0  sy 0  ty)
                   (L.V4 0  0  sz tz)
                   (L.V4 0  0  0  1)
