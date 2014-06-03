module Engine.Graphics.Render.Stencil(renderToStencil) where

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



renderSilhouettedObjects :: TransformationMatrix -> TransformationMatrix -> PointLight -> GLUtil.ShaderProgram -> [Object] -> IO ()
renderSilhouettedObjects projMat viewMat pl prog os =
    do depthFunc $= Just Lequal
       depthMask $= Enabled
       cullFace  $= Just Back
       drawBuffer $= BackBuffers


       lineWidth $= 8
       mapM_ (renderSilhouettedObject projMat viewMat pl prog) os
       checkError "renderSilhouettedObjects"


renderSilhouettedObject :: TransformationMatrix -> TransformationMatrix -> PointLight -> GLUtil.ShaderProgram -> Object -> IO ()
renderSilhouettedObject projMat viewMat pl prog o =
  mapM_ (renderSilhouettedEntity projMat viewMat objMat pl prog) $ oEntities o
      where
        objMat = mkTransMat o

renderSilhouettedEntity :: TransformationMatrix -> TransformationMatrix -> TransformationMatrix -> PointLight -> GLUtil.ShaderProgram -> Entity -> IO ()
renderSilhouettedEntity projMat viewMat objMat pl prog e =
    do currentProgram $= (Just $ GLUtil.program prog)

       bindVertexArrayObject $= Just vao

       GLUtil.asUniform mvp           $ GLUtil.getUniform prog "MVP"
       GLUtil.asUniform modelMat      $ GLUtil.getUniform prog "M"
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
       currentProgram                $= Nothing
       bindVertexArrayObject         $= Nothing
    where
      entMat = mkTransMat e
      modelMat = objMat L.!*! entMat
      mvp = projMat L.!*! viewMat L.!*! modelMat

      lightPosition = plPosition pl

      geometry = eGeometry e


      verts = gVertices geometry
      elems = gTriAdjElems geometry -- important
      nofTris = gNOFTris geometry * 2
      vao = gVAO geometry

      vPosition = GLUtil.getAttrib prog "v_position"
