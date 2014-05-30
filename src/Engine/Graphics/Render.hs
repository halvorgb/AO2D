module Engine.Graphics.Render(renderObjects) where

import Graphics.Rendering.OpenGL
import qualified Graphics.GLUtil as GLUtil
import qualified Graphics.GLUtil.Camera3D as GLUtilC
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L

import Model.Geometry
import Model.Material
import Model.Object
import Model.Entity
import Model.Camera
import Model.ClearColor
import Model.Types
import Model.World
import Model.GameState
import Model.Classes
import Model.Light

import Engine.Graphics.Common


renderObjects :: World -> GLFW.Window -> IO ()
renderObjects (gs, _) w =
    do
      clearColor $= toGLColor (gsClearColor gs)
      clear [ColorBuffer, DepthBuffer]



      (width, height) <- GLFW.getFramebufferSize w

      let cam = gsCamera gs
          [l] = gsLights gs -- TODO multiple lights..
          (projMat, viewMat) = mkProjViewMat width height cam
          ambiance = gsAmbiance gs



      -- ultra naive bullshit: draw every entity
      mapM_ (drawObject projMat viewMat ambiance l) $ gsObjects gs
--      mapM_ (drawEntityInstance projMat viewMat l) $ gsEntities gs


drawObject :: TransformationMatrix -> TransformationMatrix -> Color'RGB -> PointLight -> Object -> IO ()
drawObject projMat viewMat ambiance pl o =
  mapM_ (drawEntity projMat viewMat objMat ambiance pl) $ oEntities o
      where
        objMat = mkTransMat o

drawEntity :: TransformationMatrix -> TransformationMatrix -> TransformationMatrix -> Color'RGB -> PointLight -> Entity -> IO ()
drawEntity projMat viewMat objMat ambiance pl e = do
  currentProgram $= (Just $ GLUtil.program program)
  textureBinding Texture2D $= Just diff_map

  bindVertexArrayObject $= Just vao
  -- load uniforms
  GLUtil.asUniform mvp             $ GLUtil.getUniform program "MVP"
  GLUtil.asUniform modelMat        $ GLUtil.getUniform program "M"
  GLUtil.asUniform viewMat         $ GLUtil.getUniform program "V"
  GLUtil.asUniform ambiance        $ GLUtil.getUniform program "ambiance"
  GLUtil.asUniform lightPosition   $ GLUtil.getUniform program "lightPosition"
  GLUtil.asUniform lightColor      $ GLUtil.getUniform program "lightColor"
  GLUtil.asUniform lightStrength   $ GLUtil.getUniform program "lightStrength"
  checkError "loadUniforms"

  -- load vertex attrib data:
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


  -- Draw!
  GLUtil.drawIndexedTris nofTris

  -- disable buffers
  vertexAttribArray vPosition $= Disabled
  vertexAttribArray vUV       $= Disabled
  vertexAttribArray vNorm     $= Disabled

  bindBuffer ElementArrayBuffer $= Nothing
  textureBinding Texture2D      $= Nothing
  currentProgram                $= Nothing
  bindVertexArrayObject         $= Nothing
    where

      entMat = mkTransMat e
      modelMat = objMat L.!*! entMat
      mvp = projMat L.!*! viewMat L.!*! modelMat

      lightPosition = plPosition pl
      lightColor    = plColor pl
      lightStrength = plStrength pl


      program  = eShader e
      geometry = eGeometry e
      material = eMaterial e

      verts = gVertices geometry
      uvs   = gUVCoords geometry
      norms = gNormals  geometry
      elems = gElements geometry
      nofTris = gNOFTris geometry
      vao = gVAO geometry

      diff_map = mDiffuseMap material


      vPosition = GLUtil.getAttrib program "v_position"
      vNorm     = GLUtil.getAttrib program "v_norm"
      vUV       = GLUtil.getAttrib program "v_UV"


{-

drawEntityInstance :: L.M44 GLfloat -> L.M44 GLfloat -> State -> ML.Light -> EntityInstance -> IO ()
drawEntityInstance  projMat viewMat (_, _, resState) l ei = do
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

  GLUtil.asUniform ambiance $ GLUtil.getUniform program "ambiance"
  GLUtil.asUniform color_override $ GLUtil.getUniform program "color_override"

  GLUtil.asUniform lightPosition $ GLUtil.getUniform program "lightPosition"
  GLUtil.asUniform lightColor $ GLUtil.getUniform program "lightColor"
  GLUtil.asUniform lightStrength $ GLUtil.getUniform program "lightStrength"



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
      ambiance :: L.V3 GLfloat
      ambiance = L.V3 0.1 0.1 0.1

      lightStrength = ML.lStrength l
      lightPosition = ML.lPosition l
      lightColor    = ML.lColor    l

      color_override = Maybe.fromMaybe (eColor e) $ eiColorOverride ei

      mvp = projMat L.!*! viewMat L.!*! modelMat



      modelMat :: L.M44 GLfloat
      modelMat = L.mkTransformationMat modelScale modelTrans

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
-}

mkProjViewMat :: Int -> Int -> Camera -> (TransformationMatrix, TransformationMatrix)
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
