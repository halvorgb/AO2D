module Model.ClearColor(ClearColor(..), toGLColor, defaultClearColor, interpolateColor) where

import qualified Graphics.Rendering.OpenGL as GL

import qualified Linear as L

type Color = L.V3 Double

redCC, greenCC, blueCC :: Color
redCC    = L.V3 0.30 0.15 0.15
greenCC  = L.V3 0.25 0.45 0.1
blueCC   = L.V3 0.1 0.15 0.35

blackCC  = L.V3 0 0 0
whiteCC  = L.V3 1 1 1
pinkCC   = L.V3 1 0 1


data ClearColor =
    ClearColor {
      ccColorCycle :: [Color],
      ccClearColor :: Color,
      ccInterpRate :: Double -- how much time (in seconds) needed to change 1 value of (R,G,B)
    } deriving(Show)

toGLColor :: ClearColor -> GL.Color4 GL.GLfloat
toGLColor cc = GL.Color4 r g b 1.0
    where (L.V3 r g b) = fmap realToFrac $ ccClearColor cc

defaultClearColor :: ClearColor
defaultClearColor = ClearColor (cycle [redCC, greenCC, blueCC]) redCC 0.5
--defaultClearColor = ClearColor (cycle [blackCC, whiteCC]) blackCC 100 discoCancer

interpolateColor :: ClearColor -> Double -> ClearColor
interpolateColor cc delta
    | withinMarginOfError currentColor targetColor = cc { ccColorCycle = nextColors }

    | otherwise = let currentColor' = interpolateColors currentColor targetColor delta interpRate
                  in cc { ccClearColor = currentColor' }

    where currentColor = ccClearColor cc
          (targetColor:nextColors) = ccColorCycle cc
          interpRate = ccInterpRate cc


withinMarginOfError :: Color -> Color -> Bool
withinMarginOfError (L.V3 r1 g1 b1) (L.V3 r2 g2 b2) =
    all within $ zip [r1,g1,b1] [r2, g2, b2]

    where within :: (Double, Double) -> Bool
          within (c1, c2) = abs (c1 - c2) < 0.05


interpolateColors :: Color -> Color -> Double -> Double -> Color
interpolateColors (L.V3 r1 g1 b1) (L.V3 r2 g2 b2) delta interpRate =
    L.V3 r' g' b'
    where r' = interp r1 r2
          g' = interp g1 g2
          b' = interp b1 b2

          interp :: Double -> Double -> Double
          interp c1 c2 = c1 + diff * delta * interpRate
              where diff = c2 - c1
