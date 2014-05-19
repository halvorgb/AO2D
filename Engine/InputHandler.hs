module Engine.InputHandler(keyCallback) where

import Prelude hiding (Left, Right)

import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as Mb
import Model.State.Input



keyMap :: M.Map Input GLFW.Key
keyMap = M.fromList [(Up, GLFW.Key'W),
                     (Down, GLFW.Key'S),
                     (Right, GLFW.Key'D),
                     (Left, GLFW.Key'A),
                     (Forward, GLFW.Key'Q),
                     (Backward, GLFW.Key'Z)]




-- the order here matters, the keycheck will short circuit on success.
keyCallback :: IORef InputState ->  GLFW.KeyCallback
keyCallback inputStateIO window key scancode action mods
    | keyPressed key action GLFW.Key'Escape
        = GLFW.setWindowShouldClose window True
    | otherwise
        = do is <- readIORef inputStateIO
             let is' = setInput is key action
                 is'' = (stillPressed key action) is'
             writeIORef inputStateIO is''



setInput :: [Input] -> GLFW.Key -> GLFW.KeyState -> [Input]
setInput input key action
    | keyPressed key action GLFW.Key'W
       = addInput Up input
    | keyPressed key action GLFW.Key'S
       = addInput Down input

    | keyPressed key action GLFW.Key'D
       = addInput Right input
    | keyPressed key action GLFW.Key'A
       = addInput Left input

    | keyPressed key action GLFW.Key'Q
       = addInput Forward input

    | keyPressed key action GLFW.Key'Z
       = addInput Backward input
    | otherwise = input


-- saves some bloat.
keyPressed :: GLFW.Key -> GLFW.KeyState -> GLFW.Key -> Bool
keyPressed key keyState target =
    key == target &&
    keyState == GLFW.KeyState'Pressed

keyReleased :: GLFW.Key -> GLFW.KeyState -> GLFW.Key -> Bool
keyReleased key keyState target =
    key == target &&
    keyState == GLFW.KeyState'Released



addInput :: Input -> [Input] -> [Input]
addInput i is = (i:is)

stillPressed :: GLFW.Key -> GLFW.KeyState -> [Input] -> [Input]
stillPressed key action = L.filter (\i -> not $ keyReleased key action (Mb.fromJust $ M.lookup i keyMap))
