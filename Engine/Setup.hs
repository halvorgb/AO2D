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
import Engine.Errors
import Engine.Buffer

import Model.State
import Model.State.Resources
import Model.ShaderProgram

import Lib.LoadShaders


-- template: https://github.com/alpmestan/glfw-b-quick-example

setupEngine :: Int -> Int -> String -> State -> Resources -> IO ()
setupEngine w h winTitle state@(_, inputState, resourceState) resourcesToLoad = do
  GLFW.setErrorCallback (Just errorCallback)

  successfulInit <- GLFW.init

  mapM_ GLFW.windowHint
            [ GLFW.WindowHint'ContextVersionMajor  3,
              GLFW.WindowHint'ContextVersionMinor  3,
              GLFW.WindowHint'OpenGLForwardCompat True,
              GLFW.WindowHint'OpenGLDebugContext True,
              GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core ]
  checkError "windowHint"

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



              cullFace   $= Nothing
              depthFunc  $= Just Less
              clearColor $= Color4 0.15 0.20 0.35 1
              -- load all shaders
              loadResources resourceState resourcesToLoad
              -- initialize buffer!
              descriptor <- initBuffer

              checkError "initializing..."
              -- mainLoop
              mainLoop state descriptor window

              -- mainLoop complete, exit.
              GLFW.destroyWindow window
              GLFW.terminate
              exitSuccess

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback err = hPutStrLn stderr


loadResources :: IORef LoadedResources -> Resources -> IO ()
loadResources resState resToLoad = do
-- some debug printing
  r1 <- readIORef resState
  print r1
  mapM_ (loadShader resState) $ rShaderPrograms resToLoad
--  mapM_ (initBuffer resState) $ rObjects resToLoad

  r2 <- readIORef resState
  print r2

--  mapM_ (loadTexture resState) $ rTextures resToLoad


loadShader :: IORef LoadedResources -> ShaderProgramResource -> IO ()
loadShader resState shaderRes  = do
  program <- loadShaders [vert, frag]
  let newShaderProgram =
          ShaderProgram un program

  currentProgram $= Just program -- TODO: remove this
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
