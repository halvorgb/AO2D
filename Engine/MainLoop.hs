module Engine.MainLoop(mainLoop) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad

import Model.State

import Game.Update

import Engine.Render

mainLoop :: State -> GLFW.Window  -> IO ()
mainLoop s w = do
  close <- GLFW.windowShouldClose w
  unless close $ do

    -- update the game!
    Just delta <- GLFW.getTime
    updateGame s delta
    GLFW.setTime 0


    -- draw Everything!
    --    renderEngineState vs cs w
    renderObjects s w


    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop s w
