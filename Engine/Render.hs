module Engine.Render(renderEngineState) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW -- not needed when I remove time

import Model.State.Engine


renderEngineState :: EngineState -> IO ()
renderEngineState (EngineState vertices colors time) =
    do

       -- this is bad, but keeps the logic of the original example I guess
      Just t <- GLFW.getTime
      rotate ((realToFrac t) * 50) $ (Vector3 0 0 1 :: Vector3 GLdouble)

      renderPrimitive Triangles $ do
        color  (Color3 1 0 0 :: Color3 GLdouble)
        vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 1 0 :: Color3 GLdouble)
        vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 0 1 :: Color3 GLdouble)
        vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)
