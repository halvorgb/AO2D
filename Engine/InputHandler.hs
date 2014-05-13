module Engine.InputHandler(keyCallback) where

import Prelude hiding (Left, Right)

import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import Model.State.Input



keyMap :: M.Map Input GLFW.Key
keyMap = M.fromList [(Up, GLFW.Key'W),
                     (Left, GLFW.Key'A),
                     (Right, GLFW.Key'D),
                     (Down, GLFW.Key'S)]


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
        = do is <- readIORef inputStateIO
             maybe (return ()) -- no key was pressed.
                   (\state -> if keyReleased key action (Mb.fromJust $ M.lookup state keyMap) -- if the key that was pressed still is
                              then writeIORef inputStateIO Nothing -- released, clear variable
                              else writeIORef inputStateIO $ Just state) -- still pressed, rewrite to stop the variable from clearing
                   is


-- saves some bloat.
keyPressed :: GLFW.Key -> GLFW.KeyState -> GLFW.Key -> Bool
keyPressed key keyState target =
    key == target &&
    keyState == GLFW.KeyState'Pressed

keyReleased :: GLFW.Key -> GLFW.KeyState -> GLFW.Key -> Bool
keyReleased key keyState target =
    key == target &&
    keyState == GLFW.KeyState'Released
