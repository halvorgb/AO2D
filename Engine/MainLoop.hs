module Engine.MainLoop(mainLoop) where

import Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Control.Monad

import Model.State
import Model.Object

import Game.Update

import Engine.Render
import Engine.Errors

mainLoop :: State -> GLFW.Window -> (Double -> IO ([Vertex3 GLdouble], [Color3 GLdouble])) -> IO ()
mainLoop s w updateFunc = do
  close <- GLFW.windowShouldClose w
  unless close $ do
    -- update the game!
    Just delta <- GLFW.getTime
    (vs, cs) <- updateFunc delta
    GLFW.setTime 0

    -- draw Everything!
    --    renderEngineState vs cs w
    renderObjects s w

    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop s w updateFunc
