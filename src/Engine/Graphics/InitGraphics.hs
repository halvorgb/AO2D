module Engine.Graphics.InitGraphics(initGraphics) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit


import Engine.InputHandler
import Engine.MainLoop
import Engine.Graphics.Common
import Engine.Graphics.Assets.Resources


import Model.State
import Model.State.Resources



initGraphics :: Int -> Int -> String -> State -> Resources -> IO ()
initGraphics w h winTitle state@(_, inputState, resourceState) resourcesToLoad = do
  GLFW.setErrorCallback (Just errorCallback)

  successfulInit <- GLFW.init

  mapM_ GLFW.windowHint
            [ GLFW.WindowHint'ContextVersionMajor  3,
              GLFW.WindowHint'ContextVersionMinor  3,
              GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core,
              GLFW.WindowHint'OpenGLDebugContext True,
              GLFW.WindowHint'DepthBits 24
            ]
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

              cullFace   $= Just Back
              depthFunc  $= Just Less

              -- load all shaders
              loadResources resourceState resourcesToLoad

              checkError "initializing..."
              -- mainLoop
              mainLoop state window

              -- mainLoop complete, exit.
              GLFW.destroyWindow window
              GLFW.terminate
              exitSuccess
