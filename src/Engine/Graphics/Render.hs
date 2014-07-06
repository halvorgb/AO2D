module Engine.Graphics.Render(render) where

import Graphics.Rendering.OpenGL
import qualified Graphics.GLUtil.Camera3D as GLUtilC
import qualified Graphics.UI.GLFW as GLFW

import qualified Linear as L

import Model.Camera
import Model.ClearColor
import Model.Types
import Model.World
import Model.GameState
import Model.ShaderPrograms

import Engine.Graphics.Render.Light
import Engine.Graphics.Render.ShadowVolume


render :: World -> GLFW.Window -> IO ()
render (gs, _) w =
    do clearColor $= toGLColor (gsClearColor gs)


       (width, height) <- GLFW.getFramebufferSize w

       let cam = gsCamera gs
           [l] = gsLights gs -- TODO multiple lights..

           camPos = cPosition cam
           viewProjMat = mkViewProjMat width height cam
           ambiance = gsAmbiance gs

           sp = gsShaderPrograms gs
           lightShader     = spLight sp
           shadowVolShader = spShadowVol sp


           objects = gsObjects gs
       {-
         Wikipedia algorithm (Depth Fail):
         Disable writes to the depth and color buffers.
         Use front-face culling.
         Set the stencil operation to increment on depth fail (only count shadows behind the object).
         Render the shadow volumes.
         Use back-face culling.
         Set the stencil operation to decrement on depth fail.
         Render the shadow volumes.
        -}
       depthMask $= Enabled
       colorMask $= (Color4 Enabled Enabled Enabled Enabled)
       drawBuffer $= BackBuffers
       cullFace $= Just Back
       clear [ColorBuffer, DepthBuffer, StencilBuffer]
       renderAmbientObjects viewProjMat l camPos lightShader ambiance objects


       depthMask $= Disabled
       colorMask $= (Color4 Disabled Disabled Disabled Disabled)
       cullFace $= Nothing

       stencilTest $= Enabled
       stencilFunc $= (Always, 0, 0xFF)
       stencilOpSeparate Back  $= (OpKeep, OpIncrWrap, OpKeep)
       stencilOpSeparate Front $= (OpKeep, OpDecrWrap, OpKeep)
       renderShadowVolumeToStencil viewProjMat l shadowVolShader objects


       -- -- using given stencil info.
       -- -- draw the scene as if it was completely lit in the areas not marked by the stencil buffer.
       depthMask $= Disabled
       colorMask $= (Color4 Enabled Enabled Enabled Enabled)
       stencilFunc $= (Equal, 0, 0xff)
       stencilOp $= (OpZero, OpKeep, OpKeep)
       cullFace $= Just Back
--       blend $= Enabled
--       blendEquation $= FuncAdd
--       blendFunc $= (One, One)
       renderShadowedObjects viewProjMat l camPos lightShader objects
--       blend $= Disabled

       stencilTest $= Disabled



mkViewProjMat :: Int -> Int -> Camera -> TransformationMatrix
mkViewProjMat width height camera  = projMat L.!*! viewMat
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
