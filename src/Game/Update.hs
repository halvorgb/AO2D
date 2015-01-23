module Game.Update(updateGame) where

import           Data.IORef
import qualified Linear           as L
import           Model.Camera
import           Model.ClearColor
import           Model.GameState
import           Model.InputState
import           Model.Types
import           Model.World
import           Prelude          hiding (Left, Right)

inputToTranslationVector :: KeyboardInput -> L.V3 GLfloat
inputToTranslationVector Up       = L.V3   0   1   0
inputToTranslationVector Down     = L.V3   0 (-1)  0
inputToTranslationVector Right    = L.V3   1   0   0
inputToTranslationVector Left     = L.V3 (-1)  0   0
inputToTranslationVector Forward  = L.V3   0   0 (-1)
inputToTranslationVector Backward = L.V3   0   0   1

sensitivity :: GLfloat
sensitivity = 2

move_speed :: GLfloat
move_speed = 3


-- updates the game state
updateGame :: World -> GLfloat -> IO World
updateGame (gs, isIO) delta = do
  is <- readIORef isIO

  let kb = isKeyboardInput is -- handle keyboard changes

      translation :: L.V3 GLfloat
      translation = fmap realToFrac $ (move_speed * delta) L.*^ L.normalize (foldl (\acc i -> acc L.^+^ inputToTranslationVector i) (L.V3 0 0 0) kb)

      m = isMouseInput is  -- handle mouse changes
      tilt = realToFrac $ miY m * (-delta) * (sensitivity / 10)
      pan  = realToFrac $ miX m * (-delta) * (sensitivity / 10)

      m_cleared = MouseInput 0 0
      is' = is {isMouseInput = m_cleared}

      cam  = gsCamera gs
      cam' = moveCamera translation $ rotateCamera tilt pan cam

      cc = gsClearColor gs
      cc' = interpolateColor cc delta

      gs' = gs { gsCamera = cam',
                 gsClearColor = cc'}

  writeIORef isIO is'
  return (gs', isIO)
