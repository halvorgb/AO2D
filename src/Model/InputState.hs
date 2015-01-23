module Model.InputState where

import           Model.Types

data KeyboardInput = Up | Down | Left | Right | Forward | Backward -- ++ more
                     deriving (Eq, Show, Ord)

data MouseInput =
    MouseInput { miX :: GLfloat
               , miY :: GLfloat
               }



data InputState =
    InputState { isKeyboardInput :: [KeyboardInput]
               , isMouseInput    :: MouseInput
               }
