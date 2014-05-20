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

import Engine.Errors


renderObjects :: State -> GLFW.Window -> IO ()
renderObjects state@(gameState, _, _) w =
    do
      clear [ColorBuffer, DepthBuffer]

      gs <- readIORef gameState

      (width, height) <- GLFW.getFramebufferSize w

      let projViewMat = mkProjViewMat width height
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


mkProjViewMat :: Int -> Int  -> L.M44 GLfloat
mkProjViewMat width height  = projMat L.!*! viewMat L.!*! trans
    where
      trans      = L.mkTransformationMat L.eye3 (L.V3 0 0 (-4))
      viewMat    = GLUtilC.camMatrix cam
      cam        = GLUtilC.tilt (-30) . GLUtilC.dolly (L.V3 0 2 0) $ GLUtilC.fpsCamera
      projMat    = GLUtilC.projectionMatrix (pi/4) aspect 0.1 10
      aspect     = fromIntegral width / fromIntegral height
