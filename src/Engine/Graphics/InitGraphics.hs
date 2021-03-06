module Engine.Graphics.InitGraphics(initGraphics) where

import           Engine.Graphics.Assets.Resources
import           Engine.Graphics.Common
import           Engine.InputHandler
import           Engine.MainLoop
import           Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW                 as GLFW
import           Model.World
import           System.Exit

initGraphics :: Int -> Int -> String -> InitialState -> IO ()
initGraphics w h winTitle initialState@((_, inputState), _, _)  = do
  GLFW.setErrorCallback (Just errorCallback)

  successfulInit <- GLFW.init

  mapM_ GLFW.windowHint
            [ GLFW.WindowHint'ContextVersionMajor  3
            , GLFW.WindowHint'ContextVersionMinor  3
            , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
            , GLFW.WindowHint'OpenGLDebugContext True
            , GLFW.WindowHint'StencilBits 24
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

              cullFace $= Just Back
              depthClamp $= Enabled
              depthMask $= Enabled
              depthFunc $= Just Lequal

              dumpInfo

              -- load everything, build objets and entities
              world <- loadResources initialState

              checkError "initializing..."
              -- mainLoop
              mainLoop world window

              -- mainLoop complete, exit.
              GLFW.destroyWindow window
              GLFW.terminate

              exitSuccess
