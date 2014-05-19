module Game.Update(updateGame) where

import Data.IORef
import Control.Applicative
import Control.Monad

import Prelude hiding(Left, Right)
import Graphics.Rendering.OpenGL
import qualified Linear as L


import Model.State
import Model.State.Input
import Model.State.Game
import Model.Entity


inputToTranslationVector :: Input -> L.V3 GLfloat
inputToTranslationVector Up       = L.V3   0.0   1.0   0.0
inputToTranslationVector Down     = L.V3   0.0 (-1.0)  0.0
inputToTranslationVector Right    = L.V3   1.0   0.0   0.0
inputToTranslationVector Left     = L.V3 (-1.0)  0.0   0.0
inputToTranslationVector Forward  = L.V3   0.0   0.0   1.0
inputToTranslationVector Backward = L.V3   0.0   0.0 (-1.0)


-- updates the game state
updateGame :: State -> Double -> IO ()
updateGame (gsIO, isIO, _) delta = do
  gs <- readIORef gsIO
  is <- readIORef isIO

  let translation :: L.V3 GLfloat
      translation = foldl (\acc i -> acc L.^+^ (inputToTranslationVector i)) (L.V3 0 0 0) is
      eis = gsEntities gs
      trans' = fmap (* realToFrac delta) translation
  unless (null eis) $
         do let ei = head eis
                p  = eiPosition ei
                p' = liftA2 (+) p trans'
                ei' = ei {eiPosition = p'}
            unless (null is) $
                   writeIORef gsIO gs { gsEntities = ei':tail eis }
