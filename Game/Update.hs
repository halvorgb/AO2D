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






-- updates the game state
updateGame :: State -> Double -> IO ()
updateGame (gsIO, isIO, _) delta = do
  gs <- readIORef gsIO
  is <- readIORef isIO

  let translation :: L.V3 GLfloat
      translation = case is of
                      Nothing    -> L.V3   0.0   0.0  0.0
                      Just Up    -> L.V3   0.0   1.0  0.0
                      Just Down  -> L.V3   0.0 (-1.0) 0.0
                      Just Left  -> L.V3 (-1.0)  0.0  0.0
                      Just Right -> L.V3   1.0   0.0  0.0
      eis = gsEntities gs
      trans' = fmap (* realToFrac delta) translation

  unless (null eis) $
         do let ei = head eis
                p  = eiPosition ei
                p' = liftA2 (+) p trans'
                ei' = ei {eiPosition = p'}
            unless (is == Nothing) $
                   writeIORef gsIO gs { gsEntities = ei':(tail eis) }
