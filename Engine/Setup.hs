module Engine.Setup(setupEngine) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GLUtil as GLUtil

import System.Exit
import System.IO
import System.FilePath
import Control.Monad
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map as M

import Engine.InputHandler
import Engine.MainLoop
import Engine.Errors
import Engine.Resources


import Model.State
import Model.State.Resources



import Game.Update

-- template: https://github.com/alpmestan/glfw-b-quick-example

setupEngine :: Int -> Int -> String -> State -> Resources -> IO ()
setupEngine w h winTitle state@(_, inputState, resourceState) resourcesToLoad = do
  GLFW.setErrorCallback (Just errorCallback)

  successfulInit <- GLFW.init

  mapM_ GLFW.windowHint
            [ GLFW.WindowHint'ContextVersionMajor  3,
--              GLFW.WindowHint'ContextVersionMinor  0,
              GLFW.WindowHint'ContextVersionMinor  3,
              GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core,
              GLFW.WindowHint'OpenGLDebugContext True,
              GLFW.WindowHint'DepthBits 24 ]
  checkError "windowHint"

  if not successfulInit
  then exitFailure

  else do
    mw <- GLFW.createWindow w h winTitle Nothing Nothing
    case mw of
      Nothing -> do
              GLFW.terminate
              exitFailure
      Just window -> do
              -- window creation successful, setup callbacks
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just $ keyCallback inputState)
              GLFW.setWindowSizeCallback window (Just resizeCallback)


              dumpInfo

              blend $= Enabled
              blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
              cullFace   $= Nothing
              depthFunc  $= Just Less

              clearColor $= Color4 0.1 0.1 0.1 1

              -- load all shaders
              loadResources resourceState resourcesToLoad

              checkError "initializing..."
              -- mainLoop
              mainLoop state window

              -- mainLoop complete, exit.
              GLFW.destroyWindow window
              GLFW.terminate
              exitSuccess
-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback err = hPutStrLn stderr

resizeCallback :: GLFW.WindowSizeCallback
resizeCallback w width height = do
      let ratio = fromIntegral width / fromIntegral height

      viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))


dumpInfo :: IO ()
dumpInfo = do
   let dump message var = putStrLn . ((message ++ ": ") ++) =<< get var
   dump "Vendor" vendor
   dump "Renderer" renderer
   dump "Version" glVersion
   dump "GLSL" shadingLanguageVersion
   checkError "dumpInfo"