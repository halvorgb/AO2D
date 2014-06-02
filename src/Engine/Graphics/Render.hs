module Engine.Graphics.Render(renderObjects) where

import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GLRaw
import qualified Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4 as GLRaw
import qualified Graphics.GLUtil as GLUtil
import qualified Graphics.GLUtil.Camera3D as GLUtilC
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L

import Foreign.Ptr

import Model.Geometry
import Model.Material
import Model.Object
import Model.Entity
import Model.Camera
import Model.ClearColor
import Model.Types
import Model.World
import Model.GameState
import Model.Classes
import Model.Light

import Engine.Graphics.Common




renderObjects :: World -> GLFW.Window -> IO ()
renderObjects (gs, _) w =
    do
      clearColor $= toGLColor (gsClearColor gs)

      (width, height) <- GLFW.getFramebufferSize w

      let cam = gsCamera gs
          [l] = gsLights gs -- TODO multiple lights..
          (projMat, viewMat) = mkProjViewMat width height cam
          ambiance = gsAmbiance gs

--      cullFace   $= Just Back -- unclear if these should be here
      depthFunc  $= Just Less


      -- Algorithm: First clear all buffers.
--      clear [ColorBuffer, DepthBuffer, StencilBuffer]
      clear [ColorBuffer, DepthBuffer]
      -- Then: render DEPTH to depthbuffer. ---
--      drawBuffer $= NoBuffers
      depthMask  $= Enabled
      -- mapM_ (drawObject projMat viewMat ambiance l) $ gsObjects gs


      -- Then: Render into stencilbuffer ----
--      drawBuffer $= NoBuffers
--      depthMask $= Disabled
--      cullFace $= Nothing

--      GLRaw.glEnable GLRaw.gl_STENCIL_TEST
--      stencilFunc $= (Always, 0, 0xFF)
--      stencilOpSeparate Back  $= (OpKeep, OpIncrWrap, OpKeep)
--      stencilOpSeparate Front $= (OpKeep, OpDecrWrap, OpKeep)

      -- useProgram stencilThingie. (pass as argument Maybe programOverride?
      -- mapM_ (drawObject projMat viewMat ambiance l) $ gsObjects gs
      -- turn on culling again.
      cullFace $= Nothing

      -- Then: render shadows: ------
      drawBuffer $= BackBuffers

      -- do not update stencil buffer
--      stencilOpSeparate Back $= (OpKeep, OpKeep, OpKeep)
--      stencilFunc $= (Equal, 0, 0xFF)

      -- useProgram shadowThingie (pass as argument Maybe programOverride?)
      -- mapM_ (drawObject projMat viewMat ambiance l) $ gsObjects gs

--      GLRaw.glDisable GLRaw.gl_STENCIL_TEST

      -- Then: Render Ambient Light (only)
      depthMask $= Enabled
--      GLRaw.glEnable GLRaw.gl_BLEND
--      GLRaw.glBlendEquation GLRaw.gl_FUNC_ADD
--      GLRaw.glBlendFunc GLRaw.gl_ONE GLRaw.gl_ONE

      -- mapM_ (drawObject projMat viewMat ambiance l) $ gsObjects gs


--      GLRaw.glDisable GLRaw.gl_BLEND
      mapM_ (drawObject projMat viewMat ambiance l) $ gsObjects gs


drawObject :: TransformationMatrix -> TransformationMatrix -> Color'RGB -> PointLight -> Object -> IO ()
drawObject projMat viewMat ambiance pl o =
  mapM_ (drawEntity projMat viewMat objMat ambiance pl) $ oEntities o
      where
        objMat = mkTransMat o

drawEntity :: TransformationMatrix -> TransformationMatrix -> TransformationMatrix -> Color'RGB -> PointLight -> Entity -> IO ()
drawEntity projMat viewMat objMat ambiance pl e = do
  currentProgram $= (Just $ GLUtil.program program)
--  textureBinding Texture2D $= Just diff_map

  bindVertexArrayObject $= Just vao

--  Just prog <- get currentProgram
--  acts <- get $ activeUniforms prog
--  checkError "halla"
--  print acts
  -- load uniforms
  GLUtil.asUniform mvp             $ GLUtil.getUniform program "MVP"
  GLUtil.asUniform lightPosition   $ GLUtil.getUniform program "gLightPos"
--  GLUtil.asUniform modelMat        $ GLUtil.getUniform program "M"
--  GLUtil.asUniform viewMat         $ GLUtil.getUniform program "V"
--  GLUtil.asUniform ambiance        $ GLUtil.getUniform program "ambiance"
--  GLUtil.asUniform lightPosition   $ GLUtil.getUniform program "lightPosition"
--  GLUtil.asUniform lightColor      $ GLUtil.getUniform program "lightColor"
--  GLUtil.asUniform lightStrength   $ GLUtil.getUniform program "lightStrength"
  checkError "loadUniforms"

  -- load vertex attrib data:
  vertexAttribArray vPosition   $= Enabled
  bindBuffer ArrayBuffer        $= Just verts
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 GLUtil.offset0)
  checkError "Activate Attrib v_position"

  -- vertexAttribArray vUV   $= Enabled
  -- bindBuffer ArrayBuffer  $= Just uvs
  -- vertexAttribPointer vUV $= (ToFloat, VertexArrayDescriptor 2 Float 0 GLUtil.offset0)
  -- checkError "Activate Attrib v_UV"

  -- vertexAttribArray vNorm   $= Enabled
  -- bindBuffer ArrayBuffer    $= Just norms
  -- vertexAttribPointer vNorm $= (ToFloat, VertexArrayDescriptor 3 Float 0 GLUtil.offset0)
  -- checkError "Activate Attrib v_norm"

  bindBuffer ElementArrayBuffer $= Just elems

  checkError "setup"
  -- Draw!
  --  GLUtil.drawIndexedTris nofTris
  --  GLRaw.glDrawArrays GLRaw.gl_TRIANGLES 0 nofTris
--  drawArrays Triangles 0 nofTris
--  drawElements Triangles (nofTris*3) UnsignedInt nullPtr
  GLRaw.glDrawElements GLRaw.gl_TRIANGLES_ADJACENCY nofTris GLRaw.gl_UNSIGNED_INT nullPtr
  checkError "draw"
  -- disable buffers
  vertexAttribArray vPosition $= Disabled
--  vertexAttribArray vUV       $= Disabled
--  vertexAttribArray vNorm     $= Disabled

  bindBuffer ElementArrayBuffer $= Nothing
  textureBinding Texture2D      $= Nothing
  currentProgram                $= Nothing
  bindVertexArrayObject         $= Nothing
    where
      entMat = mkTransMat e
      modelMat = objMat L.!*! entMat
      mvp = projMat L.!*! viewMat L.!*! modelMat

      lightPosition = plPosition pl
--      lightColor    = plColor pl
--      lightStrength = plStrength pl


      program  = eShader e
      geometry = eGeometry e
      material = eMaterial e

      verts = gVertices geometry
--      uvs   = gUVCoords geometry
--      norms = gNormals  geometry
      elems = gElements geometry
      nofTris = gNOFTris geometry
      vao = gVAO geometry

--      diff_map = mDiffuseMap material


      vPosition = GLUtil.getAttrib program "v_position"
--      vNorm     = GLUtil.getAttrib program "v_norm"
--      vUV       = GLUtil.getAttrib program "v_UV"



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
      nearClip   = 0.05
      farClip    = 20
