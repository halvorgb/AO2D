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
      mapM_ (drawEntityInstance projViewMat state w) $ gsEntities gs


drawEntityInstance :: L.M44 GLfloat -> State -> GLFW.Window -> EntityInstance -> IO ()
drawEntityInstance  projViewMat (_, _, resState) w ei = do
  lr <- readIORef resState
  let Just program = M.lookup shaderName $ lrShaderPrograms lr
      Just object  = M.lookup objectName $ lrObjects lr
      verts = oVertices object
      colrs = oColors   object
      elems = oElements object

      nofTris = oNOFTris object


  currentProgram $= (Just $ GLUtil.program program)


  bindBuffer ArrayBuffer $= Just verts
  bindBuffer ArrayBuffer $= Just colrs

  GLUtil.enableAttrib program "coord3d"
  GLUtil.setAttrib program "coord3d"
            ToFloat $ VertexArrayDescriptor 4 Float 0 GLUtil.offset0

  GLUtil.enableAttrib program "v_color"
  GLUtil.setAttrib program "v_color"
            ToFloat $ VertexArrayDescriptor 4 Float 0 GLUtil.offset0




  let
      modelMat :: L.M44 GLfloat
      modelMat = L.mkTransformationMat scale pos
      mvp :: L.M44 GLfloat
      mvp = projViewMat L.!*! modelMat


  GLUtil.asUniform mvp $ GLUtil.getUniform program "mvp"

  bindBuffer ElementArrayBuffer $= Just elems

  GLUtil.drawIndexedTris (fromIntegral nofTris)

  -- disable attributes again

  vertexAttribArray (GLUtil.getAttrib program "coord3d") $= Disabled
  vertexAttribArray (GLUtil.getAttrib program "v_color") $= Disabled
  checkError "rendering"


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

mkProjViewMat :: Int -> Int  -> L.M44 GLfloat
mkProjViewMat width height  = projMat L.!*! viewMat L.!*! trans
    where
      trans      = L.mkTransformationMat L.eye3 (L.V3 0 0 (-4))
      viewMat    = GLUtilC.camMatrix cam
      cam        = GLUtilC.tilt (-30) . GLUtilC.dolly (L.V3 0 2 0) $ GLUtilC.fpsCamera
      projMat    = GLUtilC.projectionMatrix (pi/4) aspect 0.1 10
      aspect     = fromIntegral width / fromIntegral height


{-

anim :: L.M44 GLfloat
anim = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
    where
      angle = 0

-}

-- used for debugging.
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

                                                               print attribLocation

                                                               checkError "no errors before this point"
                                                               vap $=! (ih, vad)
                                                               checkError "^ that statement yields an error."

                                                               newVapVal <- get vap
                                                               print newVapVal
