module Engine.Graphics.Render.Light(renderLightedObjects, renderAmbientObjects) where

import qualified Data.Maybe                           as Maybe
import           Engine.Graphics.Common
import           Foreign.Ptr                          (nullPtr)
import qualified Graphics.GLUtil                      as GLUtil
import           Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GLRaw
import qualified Linear                               as L
import           Model.Classes
import           Model.Entity
import           Model.Geometry
import           Model.Light
import           Model.Material
import           Model.Object
import           Model.Types



renderLightedObjects :: TransformationMatrix -> PointLight -> Translation -> GLUtil.ShaderProgram -> [Object] -> IO ()
renderLightedObjects viewProjMat pl camPos prog os =
    do let ambIntensity = 0.0
           difIntensity  = 1.0

       currentProgram $= (Just $ GLUtil.program prog)

       mapM_ (renderLightedObject viewProjMat ambIntensity difIntensity pl camPos prog) os

       checkError "renderShadowedObjects"



renderAmbientObjects :: TransformationMatrix -> PointLight -> Translation -> GLUtil.ShaderProgram -> GLfloat -> [Object] -> IO ()
renderAmbientObjects viewProjMat pl camPos prog ambianceIntensity os =
    do let diffuseIntensity  = 0.0

       currentProgram $= (Just $ GLUtil.program prog)
       mapM_ (renderLightedObject viewProjMat ambianceIntensity diffuseIntensity pl camPos prog) os

       checkError "renderAmbientObjects"


renderLightedObject :: TransformationMatrix -> GLfloat -> GLfloat -> PointLight -> Translation -> GLUtil.ShaderProgram -> Object -> IO ()
renderLightedObject viewProjMat ambianceIntensity diffuseIntensity pl camPos prog o =
  mapM_ (renderLightedEntity viewProjMat objMat ambianceIntensity diffuseIntensity pl camPos prog) $ oEntities o
      where
        objMat = mkTransMat o

renderLightedEntity :: TransformationMatrix -> TransformationMatrix -> GLfloat -> GLfloat -> PointLight -> Translation -> GLUtil.ShaderProgram -> Entity -> IO ()
renderLightedEntity viewProjMat objMat ambianceIntensity diffuseIntensity pl camPos prog e =
    do textureBinding Texture2D $= Just diff_map
       bindVertexArrayObject $= Just vao

       GLUtil.asUniform mvp                $ GLUtil.getUniform prog "MVP"
       GLUtil.asUniform modelMat           $ GLUtil.getUniform prog "M"
       GLUtil.asUniform camPos             $ GLUtil.getUniform prog "eyepos_worldspace"
       GLUtil.asUniform ambianceIntensity' $ GLUtil.getUniform prog "ambientIntensity"
       GLUtil.asUniform diffuseIntensity   $ GLUtil.getUniform prog "diffuseIntensity"
       GLUtil.asUniform lightPosition      $ GLUtil.getUniform prog "plPositions"
       GLUtil.asUniform lightColor         $ GLUtil.getUniform prog "plColors"

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
       bindVertexArrayObject         $= Nothing
    where
      ambianceIntensity' = Maybe.fromMaybe ambianceIntensity $ eAmbOverride e

      entMat = mkTransMat e
      modelMat = objMat L.!*! entMat
      mvp = viewProjMat L.!*! modelMat

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
