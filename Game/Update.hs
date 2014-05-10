module Game.Update(updateGame) where

import Data.IORef

import Model.State
import Model.State.Input
import Model.State.Game


import Graphics.Rendering.OpenGL



-- updates the game state
updateGame :: State -> Double -> IO ([Vertex3 GLdouble], [Color3 GLdouble])
updateGame (gsIO, isIO) delta = do
  gs <- readIORef gsIO
  is <- readIORef isIO

  print delta

  return $ buildPrimitives gs


-- converts gameState to openGL primitives.
buildPrimitives :: GameState -> ([Vertex3 GLdouble], [Color3 GLdouble])
buildPrimitives gs = (vertices, colors)
      where
        colors = [Color3 1 0 0,
                  Color3 0 1 0,
                  Color3 0 0 1]

        vertices = [Vertex3 (-0.6) (-0.4) 0,
                    Vertex3 0.6 (-0.4) 0,
                    Vertex3 0 0.6 0]

{-
      renderPrimitive Triangles $ do
        color  (Color3 1 0 0 :: Color3 GLdouble)
        vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 1 0 :: Color3 GLdouble)
        vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 0 1 :: Color3 GLdouble)
        vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)
-}
