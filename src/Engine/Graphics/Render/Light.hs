module Engine.Graphics.Render.Light(renderLightedObjects) where

import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GLRaw
import qualified Graphics.GLUtil as GLUtil
import Graphics.Rendering.OpenGL

import qualified Data.Maybe as Maybe
import qualified Linear as L
import Foreign.Ptr(nullPtr)

import Engine.Graphics.Common


import Model.Light
import Model.Geometry
import Model.Types
import Model.Entity
import Model.Object
import Model.Classes
import Model.Material



renderLightedObjects :: TransformationMatrix -> TransformationMatrix -> PointLight -> GLUtil.ShaderProgram -> [Object] -> IO ()
renderLightedObjects projMat viewMat pl prog os =
    do drawBuffer $= BackBuffers
       stencilOpSeparate Back $= (OpKeep, OpKeep,OpKeep)
       stencilFunc $= (Equal, 0, 0xFF)


       let ambiance = L.V3 0 0 0 -- no ambiance in this pass.

       mapM_ (renderLightedObject projMat viewMat ambiance pl prog) os
       checkError "renderLightedObjects"


       GLRaw.glDisable GLRaw.gl_STENCIL_TEST



renderLightedObject :: TransformationMatrix -> TransformationMatrix -> Color'RGB -> PointLight -> GLUtil.ShaderProgram -> Object -> IO ()
renderLightedObject projMat viewMat ambiance pl prog o =
  mapM_ (renderLightedEntity projMat viewMat objMat ambiance pl prog) $ oEntities o
      where
        objMat = mkTransMat o

renderLightedEntity :: TransformationMatrix -> TransformationMatrix -> TransformationMatrix -> Color'RGB -> PointLight -> GLUtil.ShaderProgram -> Entity -> IO ()
renderLightedEntity projMat viewMat objMat ambiance pl prog e =
    do currentProgram $= (Just $ GLUtil.program prog)
       textureBinding Texture2D $= Just diff_map
       bindVertexArrayObject $= Just vao

       GLUtil.asUniform mvp           $ GLUtil.getUniform prog "MVP"
       GLUtil.asUniform lightPosition $ GLUtil.getUniform prog "lightPosition"
       GLUtil.asUniform modelMat      $ GLUtil.getUniform prog "M"
       GLUtil.asUniform viewMat       $ GLUtil.getUniform prog "V"
       GLUtil.asUniform ambiance'     $ GLUtil.getUniform prog "ambiance"
       GLUtil.asUniform lightPosition $ GLUtil.getUniform prog "lightPosition"
       GLUtil.asUniform lightColor    $ GLUtil.getUniform prog "lightColor"
       GLUtil.asUniform lightStrength $ GLUtil.getUniform prog "lightStrength"

       vertexAttribArray vPosition   $= Enabled
       bindBuffer ArrayBuffer        $= Just verts
       vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 GLUtil.offset0)


       vertexAttribArray vUV   $= Enabled
       bindBuffer ArrayBuffer  $= Just uvs
       vertexAttribPointer vUV $= (ToFloat, VertexArrayDescriptor 2 Float 0 GLUtil.offset0)

       vertexAttribArray vNorm   $= Enabled
       bindBuffer ArrayBuffer    $= Just norms
       vertexAttribPointer vNorm $= (ToFloat, VertexArrayDescriptor 3 Float 0 GLUtil.offset0)



       bindBuffer ElementArrayBuffer $= Just elems

       GLRaw.glDrawElements
            GLRaw.gl_TRIANGLES
            nofTris
            GLRaw.gl_UNSIGNED_INT nullPtr

       vertexAttribArray vPosition $= Disabled
       vertexAttribArray vUV       $= Disabled
       vertexAttribArray vNorm     $= Disabled

       bindBuffer ElementArrayBuffer $= Nothing
       textureBinding Texture2D      $= Nothing
       currentProgram                $= Nothing
       bindVertexArrayObject         $= Nothing
    where
      ambiance' = Maybe.fromMaybe ambiance $ eAmbOverride e

      entMat = mkTransMat e
      modelMat = objMat L.!*! entMat
      mvp = projMat L.!*! viewMat L.!*! modelMat

      lightPosition = plPosition pl
      lightColor    = plColor pl
      lightStrength = plStrength pl

      geometry = eGeometry e
      material = eMaterial e

      verts = gVertices geometry
      uvs   = gUVCoords geometry
      norms = gNormals  geometry
      elems = gTriElems geometry -- important
      nofTris = gNOFTris geometry
      vao = gVAO geometry

      diff_map = mDiffuseMap material

      vPosition = GLUtil.getAttrib prog "v_position"
      vNorm     = GLUtil.getAttrib prog "v_norm"
      vUV       = GLUtil.getAttrib prog "v_UV"
