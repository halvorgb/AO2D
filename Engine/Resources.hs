module Engine.Resources(loadResources) where

import qualified Graphics.GLUtil as GLUtil
import Graphics.Rendering.OpenGL

import Data.IORef
import qualified Data.Map as M

import Model.ShaderProgram
import Model.Object
import Model.State.Resources




loadResources :: IORef LoadedResources -> Resources -> IO ()
loadResources resState resToLoad = do

  mapM_ (loadShader resState) $ rShaderPrograms resToLoad
  mapM_ (loadObject resState) $ rObjects resToLoad
--  mapM_ (loadTexture resState) $ rTextures resToLoad


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
  verts <- GLUtil.fromSource ArrayBuffer        $ vs
  colrs <- GLUtil.fromSource ArrayBuffer        $ cs
  elems <- GLUtil.fromSource ElementArrayBuffer $ es
  let nofTris = length es

  modifyIORef resState
              (\ldRs -> let m = lrObjects ldRs
                        in ldRs {lrObjects =
                                     M.insert un (Object verts colrs elems nofTris) m}
              )
    where
      un = orUniqueName objRes
      vs = orVertices   objRes
      es = orElements   objRes
      cs = orColors     objRes
