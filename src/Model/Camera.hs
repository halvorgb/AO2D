module Model.Camera where

import qualified Linear      as L
import           Model.Types ()
import           Model.Types

data Camera =
    Camera { cPosition :: Translation
           , cPan      :: GLfloat -- radians
           , cTilt     :: GLfloat -- radians
           , cFov      :: GLfloat -- radians
           }

instance Show Camera where
    show c = "Pos: " ++ show (cPosition c) ++ ", pan: " ++ show (cPan c) ++ ", tilt: " ++ show (cTilt c) ++ ", fov: " ++ show (cFov c)

-- the moveBy argument is relative to the direction the camera is facing.
-- does the necessary rotations.


rotateCamera :: GLfloat -> GLfloat -> Camera -> Camera
rotateCamera add_tilt add_pan cam = cam { cTilt = tilt'
                                        , cPan = pan'}
  where tilt = cTilt cam
        pan = cPan cam
        pi2 = 2*pi

        tilt'
          | add_tilt == 0  = tilt
          | t > maxTilt    = maxTilt
          | t < (-maxTilt) = -maxTilt
          | otherwise = t -- change
          where t = tilt + add_tilt
                maxTilt = pi/2
        pan' -- avoid pan' growing over 360 degrees or 2pi
          | add_pan == 0 = pan
          | p > pi2      = p - pi2
          | p < (-pi2)   = p + pi2
          | otherwise    = p
          where p = pan + add_pan




moveCamera :: Translation -> Camera -> Camera
moveCamera (L.V3 0 0 0) cam = cam
moveCamera moveBy cam = cam {cPosition = pos'}
  where pos = cPosition cam
        pan = cPan cam
        tilt = cTilt cam

        panRotationMatrix :: L.M33 GLfloat
        panRotationMatrix =  L.V3
                             (L.V3 (cos pan) 0 (sin pan))
                             (L.V3 0 1 0)
                             (L.V3 (-sin pan) 0 (cos pan))


        tiltRotationMatrix :: L.M33 GLfloat
        tiltRotationMatrix = L.V3
                             (L.V3 1 0 0)
                             (L.V3 0 (cos tilt) (-sin tilt))
                             (L.V3 0 (sin tilt) (cos tilt))


        rotationMatrix :: TransformationMatrix
        rotationMatrix = L.mkTransformationMat (panRotationMatrix L.!*! tiltRotationMatrix) (L.V3 0 0 0)


        -- moveBy rotated to fit the camera angles.
        trans :: L.V4 GLfloat
        trans = rotationMatrix L.!* increaseDim moveBy



        -- finally translate position by moveBy'
        pos' = pos L.^+^ reduceDim trans




        reduceDim (L.V4 x y z _) = L.V3 x y z
        increaseDim (L.V3 x y z) = L.V4 x y z 0
