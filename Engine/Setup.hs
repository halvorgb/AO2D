module Engine.Setup(setupEngine) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import System.IO
import System.FilePath
import Control.Monad
import qualified Data.ByteString as BS
import Data.IORef

import Engine.InputHandler
import Engine.MainLoop

import Model.State
import Model.State.Resources
import Model.ShaderProgram

import Lib.LoadShaders


-- template: https://github.com/alpmestan/glfw-b-quick-example

setupEngine :: Int -> Int -> String -> State -> Resources -> IO ()
setupEngine w h winTitle state@(_, inputState, resourceState) resourcesToLoad = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init

  -- load all shaders
  loadResources resourcesToLoad resourceState

  if not successfulInit
  then exitFailure

  else do
    mw <- GLFW.createWindow w h winTitle Nothing Nothing
    case mw of
      Nothing -> GLFW.terminate >> exitFailure
      Just window -> do
              -- window creation successful, setup callbacks
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just $ keyCallback inputState)

              -- mainLoop
              mainLoop state window

              -- mainLoop complete, exit.
              GLFW.destroyWindow window
              GLFW.terminate
              exitSuccess

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback err description = hPutStrLn stderr description


loadResources :: Resources -> IORef LoadedResources -> IO ()
loadResources resToLoad resState = do
-- some debug printing
  r1 <- readIORef resState
  print r1
  mapM_ (loadShader resState) $ rShaderPrograms resToLoad
  r2 <- readIORef resState
  print r2

--  mapM_ (loadTexture resState) $ rTextures resToLoad


loadShader :: IORef LoadedResources -> ShaderProgramResource -> IO ()
loadShader resState shaderRes  = do
  program <- loadShaders [vert, frag]
  let newShaderProgram =
          ShaderProgram un program
  modifyIORef resState
                  (\ldRs -> let loadedProgs = lrShaderPrograms ldRs
                            in ldRs { lrShaderPrograms =
                                          newShaderProgram:loadedProgs
                                    })

  currentProgram $= Just program
      where
        un = sprUniqueName shaderRes
        vert = sprVertShader shaderRes
        frag = sprFragShader shaderRes
