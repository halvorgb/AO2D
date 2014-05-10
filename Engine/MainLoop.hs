module Engine.MainLoop(mainLoop) where

import Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

import Model.State

import Game.Update

import Engine.Render

mainLoop :: State -> GLFW.Window -> IO ()
mainLoop s@(_,_, engineState) w = do
  close <- GLFW.windowShouldClose w
  if close
  then return ()
  else do
    -- update the game!
    Just delta <- GLFW.getTime
    updateGame s delta
    GLFW.setTime 0


    -- draw everything!
    (width, height) <- GLFW.getFramebufferSize w
    let ratio = fromIntegral width / fromIntegral height

    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ColorBuffer]

    matrixMode $= Projection
    loadIdentity
    ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    translate (Vector3 1 1 1 :: Vector3 GLdouble)
    matrixMode $= Modelview 0

    loadIdentity


    -- no need to let the rendering func change state.
    esSafe <- readIORef engineState
    renderEngineState esSafe

    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop s w
