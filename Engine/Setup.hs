module Engine.Setup(setupEngine) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import System.IO
import Data.IORef


import Engine.InputHandler
import Engine.MainLoop

import Model.State
import Model.State.Engine


-- template: https://github.com/alpmestan/glfw-b-quick-example

setupEngine :: Int -> Int -> String -> State -> IO ()
setupEngine w h winTitle state@(_, inputState, _) = do

  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
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
