module Engine.InputHandler(keyCallback, cursorCallback, mouseButtonCallback) where

import           Control.Monad
import           Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import           Model.InputState
import           Prelude          hiding (Left, Right)

mouseButtonCallback :: GLFW.MouseButtonCallback
mouseButtonCallback window button action _
    | button == GLFW.MouseButton'2 &&
      action == GLFW.MouseButtonState'Pressed
          = GLFW.setCursorInputMode window GLFW.CursorInputMode'Normal

    | button == GLFW.MouseButton'1 &&
      action == GLFW.MouseButtonState'Pressed
          = GLFW.setCursorInputMode window GLFW.CursorInputMode'Hidden

    | otherwise = return ()

cursorCallback :: IORef InputState -> GLFW.CursorPosCallback
cursorCallback inputStateIO window x y =
    do
      cursorState <- GLFW.getCursorInputMode window
      unless (cursorState == GLFW.CursorInputMode'Normal) $
             do

               (w,h) <- GLFW.getFramebufferSize window
               let (mid_x, mid_y) = (fromIntegral w / 2, fromIntegral h / 2)
                   (delta_x, delta_y) = (x - mid_x, y - mid_y)

               GLFW.setCursorPos window mid_x mid_y -- reset cursorPos

               is <- readIORef inputStateIO
               let m = isMouseInput is
                   mx = miX m
                   my = miY m

                   m' = MouseInput { miX = mx + realToFrac delta_x,
                                     miY = my + realToFrac delta_y
                                   }
                   is' = is {isMouseInput = m'}
               writeIORef inputStateIO is' -- write updated mouse input!





keyCallback :: IORef InputState ->  GLFW.KeyCallback
keyCallback inputStateIO window key _ action _
    | keyPressed key action GLFW.Key'Escape
        = GLFW.setWindowShouldClose window True
    | otherwise
        = do is <- readIORef inputStateIO
             let kb = setKeyInput (isKeyboardInput is) key action
                 kb' = stillPressed key action kb
                 is' = is { isKeyboardInput = kb'}
             writeIORef inputStateIO is'

key2glfwKey :: KeyboardInput -> GLFW.Key
key2glfwKey Up = GLFW.Key'Q
key2glfwKey Down = GLFW.Key'Z
key2glfwKey Right = GLFW.Key'D
key2glfwKey Left = GLFW.Key'A
key2glfwKey Forward = GLFW.Key'W
key2glfwKey Backward = GLFW.Key'S

setKeyInput :: [KeyboardInput] -> GLFW.Key -> GLFW.KeyState -> [KeyboardInput]
setKeyInput input key action
    | keyPressed key action GLFW.Key'Q
       = addInput Up input
    | keyPressed key action GLFW.Key'Z
       = addInput Down input

    | keyPressed key action GLFW.Key'D
       = addInput Right input
    | keyPressed key action GLFW.Key'A
       = addInput Left input

    | keyPressed key action GLFW.Key'W
       = addInput Forward input

    | keyPressed key action GLFW.Key'S
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



addInput :: KeyboardInput -> [KeyboardInput] -> [KeyboardInput]
addInput i is = i:is

stillPressed :: GLFW.Key -> GLFW.KeyState -> [KeyboardInput] -> [KeyboardInput]
stillPressed key action = filter (not . keyReleased key action .key2glfwKey)
