module Engine.Render(renderObjects) where

import qualified Graphics.GLUtil as GLUtil
import qualified Graphics.GLUtil.Camera3D as GLUtilC
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L

import System.Random -- TODO: remove
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
      -- Handle resizing!
      (width, height) <- GLFW.getFramebufferSize w
      let ratio = fromIntegral width / fromIntegral height


      viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
      clear [ColorBuffer, DepthBuffer]

      gs <- readIORef gameState

      -- camera:
      let camera = transformM width height 0
      -- ultra naive bullshit: draw every entity.
      mapM_ (drawEntityInstance camera state w) $ gsEntities gs



drawEntityInstance :: L.M44 GLfloat -> State -> GLFW.Window -> EntityInstance -> IO ()
drawEntityInstance camera (_, _, resState) w ei = do
  lr <- readIORef resState
  let Just program = M.lookup shaderName $ lrShaderPrograms lr
      Just object  = M.lookup objectName $ lrObjects lr
      verts = oVertices object
      colrs = oColors   object
      elems = oElements object

      nofTris = oNOFTris object


  currentProgram $= (Just $ GLUtil.program program)
  GLUtil.enableAttrib program "coord3d"
  GLUtil.enableAttrib program "v_color"

  bindBuffer ArrayBuffer $= Just verts
  GLUtil.setAttrib program "coord3d"
        ToFloat $ VertexArrayDescriptor 3 Float 0 GLUtil.offset0

  bindBuffer ArrayBuffer $= Just colrs
  GLUtil.setAttrib program "v_color"
        ToFloat $ VertexArrayDescriptor 3 Float 0 GLUtil.offset0


  GLUtil.asUniform camera $ GLUtil.getUniform program "mvp"

  bindBuffer ElementArrayBuffer $= Just elems
  GLUtil.drawIndexedTris (fromIntegral nofTris)


  -- disable attributes again

  vertexAttribArray (GLUtil.getAttrib program "coord3d") $= Disabled
  vertexAttribArray (GLUtil.getAttrib program "v_color") $= Disabled


  checkError "drawEI"
  print name

    where
      pos = eiPosition ei
      e = eiEntity ei
      name = eName e
      shaderName = eShaderName e
      objectName = eObjectName e




transformM :: Int -> Int -> Double -> L.M44 GLfloat
transformM width height t = projection L.!*! view L.!*! model L.!*! anim
    where
  angle      = realToFrac t * pi/4
  anim       = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
  model      = L.mkTransformationMat L.eye3 $ L.V3 0 0 (-4)
  view       = GLUtilC.camMatrix cam
  cam        = GLUtilC.tilt (-30) . GLUtilC.dolly (L.V3 0 2 0) $ GLUtilC.fpsCamera
  projection = GLUtilC.projectionMatrix (pi/4) aspect 0.1 10
  aspect     = fromIntegral width / fromIntegral height
