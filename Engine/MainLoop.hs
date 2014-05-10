module Engine.MainLoop(mainLoop) where

import Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

import Model.State

import Game.Update

import Engine.Render

mainLoop :: State -> GLFW.Window -> IO ()
mainLoop s w = do
  close <- GLFW.windowShouldClose w
  if close
  then return ()
  else do
    -- update the game!
    Just delta <- GLFW.getTime
    (vs, cs) <- updateGame s delta
    GLFW.setTime 0


    -- draw Everything!
    renderEngineState vs cs w

    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop s w
