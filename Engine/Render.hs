module Engine.Render(renderEngineState) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW -- not needed when I remove time

renderEngineState :: [Vertex3 GLdouble] -> [Color3 GLdouble] -> GLFW.Window -> IO ()
renderEngineState vertices colors w =
    do
      -- Handle resizing!
      (width, height) <- GLFW.getFramebufferSize w
      let ratio = fromIntegral width / fromIntegral height

      viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
      clear [ColorBuffer]

      matrixMode $= Projection
      loadIdentity
      ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
      translate (Vector3 0 0 0 :: Vector3 GLdouble)
      matrixMode $= Modelview 0
      loadIdentity

      let vertices_and_colors = zip vertices colors

      renderPrimitive Triangles $
                      mapM_ (\(v, c) -> do
                               color c
                               vertex v) vertices_and_colors



       {-- this is bad, but keeps the logic of the original example I guess
      Just t <- GLFW.getTime
      rotate ((realToFrac t) * 50) $ (Vector3 0 0 1 :: Vector3 GLdouble)

      renderPrimitive Triangles $ do
        color  (Color3 1 0 0 :: Color3 GLdouble)
        vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 1 0 :: Color3 GLdouble)
        vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 0 1 :: Color3 GLdouble)
        vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)
-}
