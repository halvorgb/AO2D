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
  bindBuffer ArrayBuffer $= Just verts
  GLUtil.setAttrib program "coord3d"
            ToFloat $ VertexArrayDescriptor 4 Float 0 GLUtil.offset0

  GLUtil.enableAttrib program "v_color"
  bindBuffer ArrayBuffer $= Just colrs
  GLUtil.setAttrib program "v_color"
            ToFloat $ VertexArrayDescriptor 4 Float 0 GLUtil.offset0


  let
      model :: L.M44 GLfloat
      model = L.mkTransformationMat scale pos
{-              (L.V4 0.1 0.0 0.0 (L._x pos) :: L.V4 GLfloat)
              (L.V4 0.0 0.1 0.0 (L._y pos) :: L.V4 GLfloat)
              (L.V4 0.0 0.0 0.1 (L._z pos) :: L.V4 GLfloat)
              (L.V4 0.0 0.0 0.0 1.0        :: L.V4 GLfloat) -}

      wut = camera L.!*! model L.!*! anim



  GLUtil.asUniform (wut) $ GLUtil.getUniform program "mvp"


  bindBuffer ElementArrayBuffer $= Just elems


  GLUtil.drawIndexedTris (fromIntegral nofTris)


  -- disable attributes again

  vertexAttribArray (GLUtil.getAttrib program "coord3d") $= Disabled
  vertexAttribArray (GLUtil.getAttrib program "v_color") $= Disabled



    where
      scale :: L.M33 GLfloat
      scale = case eiScaleOverride ei of
                Nothing -> let sc = eScale e
                           in sc L.*!! L.eye3
                Just sc -> sc L.*!! L.eye3
      pos = eiPosition ei
      e = eiEntity ei
      name = eName e
      shaderName = eShaderName e
      objectName = eObjectName e


transformM :: Int -> Int -> Double -> L.M44 GLfloat
transformM width height t = projection L.!*! view L.!*! trans
    where
      trans      = L.mkTransformationMat L.eye3 (L.V3 0 0 (-4))
      view       = GLUtilC.camMatrix cam
      cam        = GLUtilC.tilt (-30) . GLUtilC.dolly (L.V3 0 2 0) $ GLUtilC.fpsCamera
      projection = GLUtilC.projectionMatrix (pi/4) aspect 0.1 10
      aspect     = fromIntegral width / fromIntegral height

anim :: L.M44 GLfloat
anim = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
    where
      angle = 0


setAttrib :: GLUtil.ShaderProgram -> String ->
             IntegerHandling -> VertexArrayDescriptor a -> IO ()
setAttrib sp name ih vad = case M.lookup name $ GLUtil.attribs sp of
                             Nothing -> do
                               putStrLn "This does not happen."
                               return ()
                             Just (attribLocation, _) -> let vap = vertexAttribPointer attribLocation
                                                         in do vapVal <- get vap
                                                               print (ih,vad) -- what I want to change vap to.
                                                               print vapVal

                                                               checkError "no errors before this point"
                                                               vap $=! (ih, vad)
                                                               checkError "^ that statement yields an error."

                                                               newVapVal <- get vap
                                                               print newVapVal
