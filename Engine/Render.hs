module Engine.Render(renderObjects) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW -- not needed when I remove time

import Model.Object

import Engine.Errors

renderObjects :: Descriptor -> GLFW.Window -> IO ()
renderObjects (Descriptor triangles firstIndex numVertices) w =
    do
      -- Handle resizing!
      (width, height) <- GLFW.getFramebufferSize w
      let ratio = fromIntegral width / fromIntegral height


      viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
      clear [ColorBuffer, DepthBuffer]

      bindVertexArrayObject $= Just triangles

      drawArrays Triangles firstIndex numVertices
