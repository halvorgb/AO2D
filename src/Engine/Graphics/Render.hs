module Engine.Graphics.Render(render) where

import           Engine.Graphics.Render.Light
import           Engine.Graphics.Render.ShadowVolume
import qualified Graphics.GLUtil.Camera3D            as GLUtilC
import           Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW                    as GLFW
import qualified Linear                              as L
import           Model.Camera
import           Model.ClearColor
import           Model.GameState
import           Model.ShaderPrograms
import           Model.Types
import           Model.World


render :: World -> GLFW.Window -> IO ()
render (gs, _) w =
    do clearColor $= toGLColor (gsClearColor gs)


       (width, height) <- GLFW.getFramebufferSize w

       let cam = gsCamera gs
           lights = gsLights gs

           camPos = cPosition cam
           viewProjMat = mkViewProjMat width height cam
           ambiance = gsAmbiance gs

           sp = gsShaderPrograms gs
           lightShader     = spLight sp
           shadowVolShader = spShadowVol sp


           objects = gsObjects gs

       -- Render ambiance everywhere. Write to depth-buffer.
       depthMask $= Enabled
       clear [ColorBuffer, DepthBuffer, StencilBuffer]
       renderAmbientObjects viewProjMat (head lights) camPos lightShader ambiance objects


       -- foreach light:
       mapM_
         (\l ->
           do -- Depth fail, mark shadow volumes in the stencil buffer.
              depthMask $= Disabled
              colorMask $= (Color4 Disabled Disabled Disabled Disabled)
              cullFace $= Nothing

              stencilTest $= Enabled
              stencilFunc $= (Always, 0, 0xFF)
              stencilOpSeparate Back  $= (OpKeep, OpIncrWrap, OpKeep)
              stencilOpSeparate Front $= (OpKeep, OpDecrWrap, OpKeep)

              renderShadowVolumeToStencil viewProjMat l shadowVolShader objects


              -- using given stencil info.
              -- Draw the scene with lights.
              colorMask $= (Color4 Enabled Enabled Enabled Disabled)
              stencilFunc $= (Equal, 0, 0xff)
              stencilOpSeparate Front $= (OpKeep, OpKeep, OpKeep)
              cullFace $= Just Back
              blend $= Enabled
              blendEquation $= FuncAdd
              blendColor $= Color4 0 0 0 0.9
              blendFunc $= (ConstantAlpha, ConstantAlpha)
              renderLightedObjects viewProjMat l camPos lightShader objects
              blend $= Disabled

              stencilTest $= Disabled
         ) lights

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
      nearClip   = 0.01
      farClip    = 100
