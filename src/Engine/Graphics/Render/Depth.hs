module Engine.Graphics.Render.Depth(renderDepth) where


import           Engine.Graphics.Common
import           Foreign.Ptr                          (nullPtr)
import qualified Graphics.GLUtil                      as GLUtil
import           Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GLRaw
import qualified Linear                               as L
import           Model.Classes
import           Model.Entity
import           Model.Geometry


import           Model.Object
import           Model.Types

renderDepth :: TransformationMatrix -> GLUtil.ShaderProgram -> [Object] -> IO ()
renderDepth viewProjMat prog objects =
  do currentProgram $= (Just $ GLUtil.program prog)
     mapM_ (renderObjectToDepth viewProjMat prog) objects
     checkError "renderDepth"

renderObjectToDepth :: TransformationMatrix -> GLUtil.ShaderProgram -> Object -> IO ()
renderObjectToDepth viewProjMat prog o =
  mapM_ (renderEntityToDepth viewProjMat objMat prog) $ oEntities o
      where
        objMat = mkTransMat o

renderEntityToDepth :: TransformationMatrix -> TransformationMatrix -> GLUtil.ShaderProgram -> Entity -> IO ()
renderEntityToDepth viewProjMat objMat prog e =
  do bindVertexArrayObject $= Just vao

     GLUtil.asUniform mvp $ GLUtil.getUniform prog "MVP"

     vertexAttribArray vPosition   $= Enabled
     bindBuffer ArrayBuffer        $= Just verts
     vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 GLUtil.offset0)
     bindBuffer ElementArrayBuffer $= Just elems

     GLRaw.glDrawElements
       GLRaw.gl_TRIANGLES
       nofTris
       GLRaw.gl_UNSIGNED_INT nullPtr

     vertexAttribArray vPosition   $= Disabled
     bindBuffer ElementArrayBuffer $= Nothing
     bindVertexArrayObject         $= Nothing
  where entMat = mkTransMat e
        modelMat = objMat L.!*! entMat
        mvp = viewProjMat L.!*! modelMat

        geometry = eGeometry e
        verts = gVertices geometry
        elems = gTriElems geometry -- important
        nofTris = gNOFTris geometry
        vao = gVAO geometry

        vPosition = GLUtil.getAttrib prog "v_position"
