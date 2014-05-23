module Engine.Setup(setupEngine) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW

import System.Exit
import System.IO

import Engine.InputHandler
import Engine.MainLoop
import Engine.Errors
import Engine.Resources


import Model.State
import Model.State.Resources

-- template: https://github.com/alpmestan/glfw-b-quick-example
setupEngine :: Int -> Int -> String -> State -> Resources -> IO ()
setupEngine w h winTitle state@(_, inputState, resourceState) resourcesToLoad = do
  GLFW.setErrorCallback (Just errorCallback)

  successfulInit <- GLFW.init

  mapM_ GLFW.windowHint
            [ GLFW.WindowHint'ContextVersionMajor  3,
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
              GLFW.setKeyCallback
                  window $ Just $ keyCallback inputState
              GLFW.setWindowSizeCallback
                  window $ Just resizeCallback
              GLFW.setCursorPosCallback
                  window $ Just $ cursorCallback inputState
              GLFW.setMouseButtonCallback
                  window $ Just mouseButtonCallback

              dumpInfo

--              blend $= Enabled
--              blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
              cullFace   $= Just Back
              depthFunc  $= Just Less

              clearColor $= Color4 0.25 0.45 0.1 1

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
errorCallback _ = hPutStrLn stderr

resizeCallback :: GLFW.WindowSizeCallback
resizeCallback _ width height = viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))


dumpInfo :: IO ()
dumpInfo = do
   let dump message var = putStrLn . ((message ++ ": ") ++) =<< get var
   dump "Vendor" vendor
   dump "Renderer" renderer
   dump "Version" glVersion
   dump "GLSL" shadingLanguageVersion
   checkError "dumpInfo"
