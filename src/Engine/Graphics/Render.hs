module Engine.Graphics.Render(render) where

import Graphics.Rendering.OpenGL
import qualified Graphics.GLUtil.Camera3D as GLUtilC
import qualified Graphics.UI.GLFW as GLFW

import Model.Camera
import Model.ClearColor
import Model.Types
import Model.World
import Model.GameState
import Model.ShaderPrograms

import Engine.Graphics.Render.Light
import Engine.Graphics.Render.ShadowVolume
import Engine.Graphics.Render.Depth


render :: World -> GLFW.Window -> IO ()
render (gs, _) w =
    do clearColor $= toGLColor (gsClearColor gs)


       (width, height) <- GLFW.getFramebufferSize w

       let cam = gsCamera gs
           [l] = gsLights gs -- TODO multiple lights..

           camPos = cPosition cam
           (projMat, viewMat) = mkProjViewMat width height cam
           ambiance = gsAmbiance gs

           sp = gsShaderPrograms gs
           depthShader     = spDepth sp
           lightShader     = spLight sp
           shadowVolShader = spShadowVol sp


           objects = gsObjects gs


       clear [ColorBuffer, DepthBuffer, StencilBuffer]
       renderSceneToDepth projMat viewMat depthShader objects
       stencilTest $= Enabled
       renderShadowVolumeToStencil projMat viewMat l shadowVolShader objects
       renderShadowedObjects projMat viewMat l camPos lightShader objects
       stencilTest $= Disabled
--       renderAmbientObjects projMat viewMat l camPos lightShader ambiance objects





mkProjViewMat :: Int -> Int -> Camera -> (TransformationMatrix, TransformationMatrix)
mkProjViewMat width height camera  = (projMat, viewMat)
    where
      viewMat    = GLUtilC.camMatrix cam
      cam        = GLUtilC.panRad pan . GLUtilC.tiltRad tilt . GLUtilC.dolly pos $ GLUtilC.fpsCamera


      tilt       = cTilt camera
      pan        = cPan camera
      pos        = cPosition camera

      projMat    = GLUtilC.projectionMatrix fov aspect nearClip farClip
      aspect     = fromIntegral width / fromIntegral height
      fov        = cFov camera
      nearClip   = 1
      farClip    = 100
