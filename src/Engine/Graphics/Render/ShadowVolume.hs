module Engine.Graphics.Render.ShadowVolume(renderShadowVolumeToStencil) where

import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GLRaw
import qualified Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4 as GLRaw
import qualified Graphics.GLUtil as GLUtil
import Graphics.Rendering.OpenGL

import qualified Linear as L
import Foreign.Ptr(nullPtr)

import Engine.Graphics.Common


import Model.Light
import Model.Geometry
import Model.Types
import Model.Entity
import Model.Object
import Model.Classes



renderShadowVolumeToStencil :: TransformationMatrix ->
                               TransformationMatrix ->
                               PointLight ->
                               GLUtil.ShaderProgram -> [Object] ->
                               IO ()
renderShadowVolumeToStencil projMat viewMat pl prog os =
    do depthMask  $= Disabled
       drawBuffer $= NoBuffers

       cullFace   $= Nothing



       stencilFunc $= (Always, 0, 0xff)
       stencilOpSeparate Back  $= (OpKeep, OpIncrWrap, OpKeep)
       stencilOpSeparate Front $= (OpKeep, OpDecrWrap, OpKeep)


       currentProgram $= (Just $ GLUtil.program prog)


       mapM_ (renderObjectToStencil projMat viewMat pl prog) os
       checkError "renderShadowVolumeToStencil"

       currentProgram $= Nothing

       cullFace $= Just Back

renderObjectToStencil :: TransformationMatrix -> TransformationMatrix -> PointLight -> GLUtil.ShaderProgram -> Object -> IO ()
renderObjectToStencil projMat viewMat pl prog o =
  mapM_ (renderEntityToStencil projMat viewMat objMat pl prog) $ oEntities o
      where
        objMat = mkTransMat o

renderEntityToStencil :: TransformationMatrix -> TransformationMatrix -> TransformationMatrix -> PointLight -> GLUtil.ShaderProgram -> Entity -> IO ()
renderEntityToStencil projMat viewMat objMat pl prog e =
    do

       bindVertexArrayObject $= Just vao

       GLUtil.asUniform mvp           $ GLUtil.getUniform prog "MVP"
       GLUtil.asUniform modelMat      $ GLUtil.getUniform prog "M"
       GLUtil.asUniform vp            $ GLUtil.getUniform prog "VP"

       GLUtil.asUniform lightPosition $ GLUtil.getUniform prog "lightPosition"

       vertexAttribArray vPosition   $= Enabled
       bindBuffer ArrayBuffer        $= Just verts
       vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 GLUtil.offset0)

       bindBuffer ElementArrayBuffer $= Just elems

       GLRaw.glDrawElements
            GLRaw.gl_TRIANGLES_ADJACENCY
            nofTris
            GLRaw.gl_UNSIGNED_INT nullPtr

       vertexAttribArray vPosition $= Disabled

       bindBuffer ElementArrayBuffer $= Nothing
       bindVertexArrayObject         $= Nothing
    where
      entMat = mkTransMat e
      modelMat = objMat L.!*! entMat
      mvp = projMat L.!*! viewMat L.!*! modelMat


      vp = projMat L.!*! viewMat

      lightPosition = plPosition pl

      geometry = eGeometry e


      verts = gVertices geometry
      elems = gTriAdjElems geometry -- important
      nofTris = gNOFAdjs geometry
      vao = gVAO geometry

      vPosition = GLUtil.getAttrib prog "v_position"
