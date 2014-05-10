module Engine.InputHandler(keyCallback) where

import Prelude hiding (Left, Right)

import qualified Graphics.UI.GLFW as GLFW
import Data.IORef

import Model.State.Input


-- the order here matters, the keycheck will short circuit on success.
keyCallback :: IORef InputState ->  GLFW.KeyCallback
keyCallback inputStateIO window key scancode action mods
    | keyPressed key action GLFW.Key'Escape
        = do
        GLFW.setWindowShouldClose window True
        writeIORef inputStateIO Nothing

    | keyPressed key action GLFW.Key'W
       = writeIORef inputStateIO $ Just Up

    | keyPressed key action GLFW.Key'A
       = writeIORef inputStateIO $ Just Left

    | keyPressed key action GLFW.Key'S
       = writeIORef inputStateIO $ Just Down

    | keyPressed key action GLFW.Key'D
       = writeIORef inputStateIO $ Just Right

    | otherwise
        = writeIORef inputStateIO Nothing



-- saves some bloat.
keyPressed :: GLFW.Key -> GLFW.KeyState -> GLFW.Key -> Bool
keyPressed key keyState target =
    key == target &&
    keyState == GLFW.KeyState'Pressed
