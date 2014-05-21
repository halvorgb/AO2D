module Game.Update(updateGame) where

import Data.IORef
import Prelude hiding(Left, Right)

import Graphics.Rendering.OpenGL
import qualified Linear as L


import Model.State
import Model.State.Input
import Model.State.Game

import Model.Camera

inputToTranslationVector :: KeyboardInput -> L.V3 GLfloat
inputToTranslationVector Up       = L.V3   0.00   0.01   0.00
inputToTranslationVector Down     = L.V3   0.00 (-0.01)  0.00
inputToTranslationVector Right    = L.V3   0.01   0.00   0.00
inputToTranslationVector Left     = L.V3 (-0.01)  0.00   0.00
inputToTranslationVector Forward  = L.V3   0.00   0.00 (-0.01)
inputToTranslationVector Backward = L.V3   0.00   0.00   0.01

sensitivity :: Double
sensitivity = 2


-- updates the game state
updateGame :: State -> Double -> IO ()
updateGame (gsIO, isIO, _) delta = do
  gs <- readIORef gsIO
  is <- readIORef isIO

  let kb = isKeyboardInput is -- handle keyboard changes

      translation :: L.V3 GLfloat
      translation = foldl (\acc i -> acc L.^+^ (inputToTranslationVector i)) (L.V3 0 0 0) kb

      m = isMouseInput is  -- handle mouse changes
      tilt = realToFrac $ miY m * (-delta) * (sensitivity / 10)
      pan  = realToFrac $ miX m * (-delta) * (sensitivity / 10)

      m_cleared = MouseInput 0 0
      is' = is {isMouseInput = m_cleared}

      cam  = gsCamera gs
      cam' = moveCamera translation $ rotateCamera tilt pan cam

      gs' = gs { gsCamera = cam' }

  writeIORef isIO is'
  writeIORef gsIO gs'
