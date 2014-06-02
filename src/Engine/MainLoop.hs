module Engine.MainLoop(mainLoop) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad

import Model.World

import Game.Update

import Engine.Graphics.Render

mainLoop :: World -> GLFW.Window  -> IO ()
mainLoop world win = do
  close <- GLFW.windowShouldClose win
  unless close $ do

    -- update the game!
    Just delta <- GLFW.getTime
    world' <- updateGame world $ realToFrac delta
    GLFW.setTime 0


    -- draw Everything!
    render world' win


    GLFW.swapBuffers win
    GLFW.pollEvents
    mainLoop world' win
