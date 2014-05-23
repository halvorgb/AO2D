module Engine.Render(renderObjects) where

import qualified Graphics.GLUtil as GLUtil
import qualified Graphics.GLUtil.Camera3D as GLUtilC
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Data.IORef

import Model.Material
import Model.Object
import Model.State
import Model.State.Resources
import Model.State.Game
import Model.Entity
import Model.Camera

import Engine.Errors


renderObjects :: State -> GLFW.Window -> IO ()
renderObjects state@(gameState, _, _) w =
    do
      clear [ColorBuffer, DepthBuffer]

      gs <- readIORef gameState

      (width, height) <- GLFW.getFramebufferSize w

      let cam = gsCamera gs
          (projMat, viewMat) = mkProjViewMat width height cam



      -- ultra naive bullshit: draw every entity.
      mapM_ (drawEntityInstance projMat viewMat state ) $ gsEntities gs


drawEntityInstance :: L.M44 GLfloat -> L.M44 GLfloat -> State  -> EntityInstance -> IO ()
drawEntityInstance  projMat viewMat (_, _, resState) ei = do
  lr <- readIORef resState
  let Just program  = M.lookup shaderName $ lrShaderPrograms lr
      Just object   = M.lookup objectName $ lrObjects lr
      Just material = M.lookup materialName $ lrMaterials lr
      verts = oVertices object
      uvs   = oUV   object
      norms = oNormals object
      elems = oElements object -- never used?

      nofTris = oNOFTris object

      vao = oVAO object


      material_diffuse = mTextureObject material

 -- enable VAO:
  bindVertexArrayObject $= Just vao

  currentProgram $= (Just $ GLUtil.program program)
  textureBinding Texture2D $= Just material_diffuse

  GLUtil.asUniform mvp $ GLUtil.getUniform program "MVP"
  GLUtil.asUniform modelMat $ GLUtil.getUniform program "M"
  GLUtil.asUniform viewMat $ GLUtil.getUniform program "V"

  GLUtil.asUniform lightpos_worldspace $ GLUtil.getUniform program "lightpos_worldspace"

--  GLUtil.asUniform global_color $ GLUtil.getUniform program "global_color"

  let vPosition = GLUtil.getAttrib program "v_position"
      vUV       = GLUtil.getAttrib program "v_UV"
      vNorm     = GLUtil.getAttrib program "v_norm"


  vertexAttribArray vPosition   $= Enabled
  bindBuffer ArrayBuffer        $= Just verts
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 4 Float 0 GLUtil.offset0)
  checkError "Activate Attrib v_position"

  vertexAttribArray vUV   $= Enabled
  bindBuffer ArrayBuffer  $= Just uvs
  vertexAttribPointer vUV $= (ToFloat, VertexArrayDescriptor 2 Float 0 GLUtil.offset0)
  checkError "Activate Attrib v_UV"

  vertexAttribArray vNorm   $= Enabled
  bindBuffer ArrayBuffer    $= Just norms
  vertexAttribPointer vNorm $= (ToFloat, VertexArrayDescriptor 3 Float 0 GLUtil.offset0)
  checkError "Activate Attrib v_norm"

  bindBuffer ElementArrayBuffer $= Just elems
  GLUtil.drawIndexedTris (fromIntegral nofTris)

  -- disable attributes again
  vertexAttribArray vPosition $= Disabled
  vertexAttribArray vUV       $= Disabled
  vertexAttribArray vNorm     $= Disabled

    where
      lightpos_worldspace :: L.V3 GLfloat
      lightpos_worldspace = L.V3 1 3 1
      --      global_color = Maybe.fromMaybe (eColor e) $eiColorOverride ei


      mvp = projMat L.!*! viewMat L.!*! modelMat
      -- TODO: turn on scale again.
      modelMat :: L.M44 GLfloat
      modelMat = L.mkTransformationMat modelScale modelTrans
--      modelMat_unscaled = L.mkTransformationMat L.eye3 modelTrans

      modelScale :: L.M33 GLfloat
      modelScale = modelSc sc

      modelSc (L.V3 x y z) = L.V3
                             (L.V3 x 0 0)
                             (L.V3 0 y 0)
                             (L.V3 0 0 z)

      sc = Maybe.fromMaybe (eScale e) $ eiScaleOverride ei

      modelTrans = eiPosition ei
      e = eiEntity ei
      shaderName = eShaderName e
      objectName = eObjectName e
      materialName = eMaterialName e


mkProjViewMat :: Int -> Int -> Camera -> (L.M44 GLfloat, L.M44 GLfloat)
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
