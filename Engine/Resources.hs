module Engine.Resources(loadResources) where

import qualified Graphics.GLUtil as GLUtil
import Graphics.Rendering.OpenGL
import Data.IORef
import qualified Data.Map as M

import qualified Codec.Picture as PNG

import Engine.Errors
import Engine.ImageLoader
import Engine.ModelLoader

import Model.ShaderProgram
import Model.Object
import Model.Material
import Model.State.Resources


loadResources :: IORef LoadedResources -> Resources -> IO ()
loadResources resState resToLoad = do


  mapM_ (loadShader resState) $ rShaderPrograms resToLoad
  checkError "loadShaders"
  mapM_ (loadObject resState) $ rObjects resToLoad
  checkError "loadObjects"

  mapM_ (loadMaterial resState) $ rMaterials resToLoad
  checkError "loadMaterial"


loadShader :: IORef LoadedResources -> ShaderProgramResource -> IO ()
loadShader resState shaderRes = do
  prog <- GLUtil.simpleShaderProgram vert frag
  modifyIORef resState
                  (\ldRs -> let m = lrShaderPrograms ldRs
                            in ldRs { lrShaderPrograms =
                                          M.insert un prog m}
                  )

      where
        un = sprUniqueName shaderRes
        vert = sprVertShader shaderRes
        frag = sprFragShader shaderRes


loadObject :: IORef LoadedResources -> ObjectResource -> IO ()
loadObject resState oR@(ObjectResource un _ _) = do
  (vertexCoordinates, vertexUVCoordinates, vertexNormals, faceElements)
      <- loadModel oR

  -- Generate VAO
  [vao] <- genObjectNames 1
  bindVertexArrayObject $= Just vao
  verts <- GLUtil.fromSource ArrayBuffer         vertexCoordinates
  uvs   <- GLUtil.fromSource ArrayBuffer         vertexUVCoordinates
  elems <- GLUtil.fromSource ElementArrayBuffer  faceElements
  let nofTris = length faceElements
      obj     = Object verts uvs elems nofTris vao

  modifyIORef resState
              (\ldRs -> let m = lrObjects ldRs
                        in ldRs {lrObjects =
                                     M.insert un obj m}
              )
  bindVertexArrayObject $= Nothing


loadMaterial :: IORef LoadedResources -> MaterialResource -> IO ()
loadMaterial resState matRes = do
  -- load diffuse Image:
  diff_img <- loadImage diffuseFilePath

  let wd = fromIntegral $ PNG.imageWidth diff_img
      hd = fromIntegral $ PNG.imageHeight diff_img
      dd = PNG.imageData diff_img
      texInfo = GLUtil.TexInfo wd hd GLUtil.TexRGBA dd


  texObject' <- GLUtil.loadTexture texInfo

  textureBinding Texture2D $= Just texObject'
  let newMat =
          Material {mUniqueName = un,
                    mSize = (fromIntegral wd, fromIntegral hd),
                    mTextureObject = texObject'}

  modifyIORef resState
                  (\ldRs -> let m = lrMaterials ldRs
                            in ldRs {lrMaterials =
                                     M.insert un newMat m}
                  )



  textureFilter Texture2D $= ((Linear',Just Linear'), Linear')
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  generateMipmap' Texture2D
  textureBinding Texture2D $= Nothing
  checkError "loadTexture"
    where
      un = mrUniqueName matRes
      diffuseFilePath = mrDiffuseFilePath matRes
