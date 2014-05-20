module Engine.Resources(loadResources) where

import qualified Graphics.GLUtil as GLUtil
import Graphics.Rendering.OpenGL
import Data.IORef
import qualified Data.Map as M

import Data.Vector.Storable(unsafeWith)

import qualified Codec.Picture as PNG

import Engine.Errors
import Engine.ImageLoader

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
loadObject resState objRes = do
  -- Generate VAO
  [vao] <- genObjectNames 1
  bindVertexArrayObject $= Just vao
  verts <- GLUtil.fromSource ArrayBuffer         vs
  colrs <- GLUtil.fromSource ArrayBuffer         cs
  elems <- GLUtil.fromSource ElementArrayBuffer  es
  let nofTris = length es

  modifyIORef resState
              (\ldRs -> let m = lrObjects ldRs
                        in ldRs {lrObjects =
                                     M.insert un (Object verts colrs elems nofTris vao) m}
              )
  bindVertexArrayObject $= Nothing
    where
      un = orUniqueName objRes
      vs = orVertices   objRes
      es = orElements   objRes
      cs = orColors     objRes



loadMaterial :: IORef LoadedResources -> MaterialResource -> IO ()
loadMaterial resState matRes = do
  [texObject] <- genObjectNames 1
  textureBinding Texture2D $= Just texObject

  -- load diffuse Image:
  diffuse <- loadImage diffuseFilePath

  let wd = fromIntegral $ PNG.imageWidth diffuse
      hd = fromIntegral $ PNG.imageHeight diffuse
      dd = PNG.imageData diffuse


  --http://stackoverflow.com/questions/10468845/juicypixels-texture-loading-in-haskell-opengl

  unsafeWith dd $ (\ ptr ->
                   -- Generate the texture
                   texImage2D
                   -- Simple Texture2D w/o cube map
                   Texture2D
                   -- No proxy
                   NoProxy
                   -- No mipmaps
                   0
                   -- Internal storage format: use R8G8B8A8 as internal storage
                   RGBA8
                   -- Size of the image
                   (TextureSize2D wd hd)
                   -- No borders
                   0
                   -- The pixel data: the vector contains Bytes, in RGBA order
                   (PixelData RGBA UnsignedByte ptr)
                  )

  print "lol"
  let newMat =
          Material {mUniqueName = un,
                    mSize = (fromIntegral wd, fromIntegral hd),
                    mTextureObject = texObject}

  print "done"

    where
      un = mrUniqueName matRes
      diffuseFilePath = mrDiffuseFilePath matRes
