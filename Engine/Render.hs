module Engine.Render(renderObjects) where

import qualified Graphics.GLUtil as GLUtil
import qualified Graphics.GLUtil.Camera3D as GLUtilC
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L

import qualified Data.Map as M
import Data.IORef

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
  let Just program = M.lookup shaderName $ lrShaderPrograms lr
      Just object  = M.lookup objectName $ lrObjects lr
      verts = oVertices object
      colrs = oColors   object
--      elems = oElements object -- never used?

      nofTris = oNOFTris object

      vao = oVAO object


  currentProgram $= (Just $ GLUtil.program program)


  -- enable VAO:
  bindVertexArrayObject $= Just vao

  GLUtil.asUniform mvp $ GLUtil.getUniform program "mvp"

  let vPosition = GLUtil.getAttrib program "coord3d"
      vColor    = GLUtil.getAttrib program "v_color"


  vertexAttribArray vPosition   $= Enabled
  bindBuffer ArrayBuffer $= Just verts
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 4 Float 0 GLUtil.offset0)
  checkError "Activate Attrib vPosition"

  vertexAttribArray vColor      $= Enabled
  bindBuffer ArrayBuffer $= Just colrs
  vertexAttribPointer vColor    $= (ToFloat, VertexArrayDescriptor 4 Float 0 GLUtil.offset0)
  checkError "Activate Attrib vColor"



--  bindBuffer ElementArrayBuffer $= Just elems

  GLUtil.drawIndexedTris (fromIntegral nofTris)

  -- disable attributes again
  vertexAttribArray vColor $= Disabled
  vertexAttribArray vPosition $= Disabled

  bindVertexArrayObject $= Nothing
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


mkProjViewMat :: Int -> Int -> Camera -> L.M44 GLfloat
mkProjViewMat width height camera  = projMat L.!*! viewMat
    where
      viewMat    = GLUtilC.camMatrix cam
      cam        = GLUtilC.pan pan . GLUtilC.tilt tilt . GLUtilC.dolly pos $ GLUtilC.fpsCamera


      tilt       = cTilt camera
      pan        = cPan camera
      pos        = cPosition camera

      projMat    = GLUtilC.projectionMatrix fov aspect nearClip farClip
      aspect     = fromIntegral width / fromIntegral height
      fov        = cFov camera
      nearClip   = 0.5
      farClip    = 20
