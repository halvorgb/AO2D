module Engine.Graphics.Render.Depth(renderSceneToDepth) where

import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GLRaw
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



renderSceneToDepth :: TransformationMatrix -> TransformationMatrix  -> GLUtil.ShaderProgram -> [Object] -> IO ()
renderSceneToDepth projMat viewMat prog os =
    do cullFace $= Just Back

       currentProgram $= (Just $ GLUtil.program prog)

       mapM_ (renderObjectToDepth projMat viewMat prog) os
       checkError "renderObjectToDepth"

       currentProgram $= Nothing


renderObjectToDepth :: TransformationMatrix -> TransformationMatrix ->
                       GLUtil.ShaderProgram -> Object -> IO ()
renderObjectToDepth projMat viewMat prog o =
    mapM_ (renderEntityToDepth projMat viewMat objMat prog) $ oEntities o
        where
          objMat = mkTransMat o


renderEntityToDepth :: TransformationMatrix -> TransformationMatrix -> TransformationMatrix -> GLUtil.ShaderProgram -> Entity -> IO ()
renderEntityToDepth projMat viewMat objMat prog e =
    do bindVertexArrayObject $= Just vao

       GLUtil.asUniform mvp           $ GLUtil.getUniform prog "MVP"

       vertexAttribArray vPosition   $= Enabled
       bindBuffer ArrayBuffer        $= Just verts
       vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 GLUtil.offset0)

       bindBuffer ElementArrayBuffer $= Just elems

       GLRaw.glDrawElements
            GLRaw.gl_TRIANGLES
            nofTris
            GLRaw.gl_UNSIGNED_INT
            nullPtr

       vertexAttribArray vPosition $= Disabled

       bindBuffer ElementArrayBuffer $= Nothing
       bindVertexArrayObject         $= Nothing
    where
      entMat = mkTransMat e
      modelMat = objMat L.!*! entMat
      mvp = projMat L.!*! viewMat L.!*! modelMat


      geometry = eGeometry e


      verts = gVertices geometry
      elems = gTriElems geometry -- important
      nofTris = gNOFTris geometry
      vao = gVAO geometry

      vPosition = GLUtil.getAttrib prog "v_position"
