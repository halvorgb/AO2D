module Engine.Render(renderObjects) where

import qualified Graphics.GLUtil as GLUtil
import qualified Graphics.GLUtil.Camera3D as GLUtilC
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L

import qualified Data.Map as M
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
          projViewMat = mkProjViewMat width height cam


      -- ultra naive bullshit: draw every entity.
      mapM_ (drawEntityInstance projViewMat state ) $ gsEntities gs


drawEntityInstance :: L.M44 GLfloat -> State  -> EntityInstance -> IO ()
drawEntityInstance  projViewMat (_, _, resState) ei = do
  lr <- readIORef resState
  let Just program  = M.lookup shaderName $ lrShaderPrograms lr
      Just object   = M.lookup objectName $ lrObjects lr
      Just material = M.lookup materialName $ lrMaterials lr
      verts = oVertices object
      uvs   = oUV   object
      elems = oElements object -- never used?

      nofTris = oNOFTris object

      vao = oVAO object


      material_diffuse = mTextureObject material

 -- enable VAO:
  bindVertexArrayObject $= Just vao

  currentProgram $= (Just $ GLUtil.program program)
  textureBinding Texture2D $= (Just material_diffuse)

  GLUtil.asUniform mvp $ GLUtil.getUniform program "MVP"

  let vPosition = GLUtil.getAttrib program "v_position"
      vUV       = GLUtil.getAttrib program "v_UV"


  vertexAttribArray vPosition   $= Enabled
  bindBuffer ArrayBuffer $= Just verts
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 4 Float 0 GLUtil.offset0)
  checkError "Activate Attrib v_position"

  vertexAttribArray vUV      $= Enabled
  bindBuffer ArrayBuffer $= Just uvs
  vertexAttribPointer vUV    $= (ToFloat, VertexArrayDescriptor 2 Float 0 GLUtil.offset0)
  checkError "Activate Attrib v_UV"

  bindBuffer ElementArrayBuffer $= Just elems

  GLUtil.drawIndexedTris (fromIntegral nofTris)

  -- disable attributes again
  vertexAttribArray vUV $= Disabled
  vertexAttribArray vPosition $= Disabled

    where
      modelMat :: L.M44 GLfloat
      modelMat = L.mkTransformationMat modelScale modelTrans

      mvp :: L.M44 GLfloat
      mvp = projViewMat L.!*! modelMat

      modelScale :: L.M33 GLfloat
      modelScale = case eiScaleOverride ei of
                     Nothing -> let sc = eScale e
                                in sc L.*!! L.eye3
                     Just sc -> sc L.*!! L.eye3
      modelTrans = eiPosition ei
      e = eiEntity ei
      shaderName = eShaderName e
      objectName = eObjectName e
      materialName = eMaterialName e


mkProjViewMat :: Int -> Int -> Camera -> L.M44 GLfloat
mkProjViewMat width height camera  = projMat L.!*! viewMat
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
