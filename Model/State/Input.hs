module Model.State.Input where

data KeyboardInput = Up | Down | Left | Right | Forward | Backward -- ++ more
                     deriving (Eq, Show, Ord)

data MouseInput =
    MouseInput { miX :: Double,
                 miY :: Double }



data InputState =
    InputState { isKeyboardInput :: [KeyboardInput],
                 isMouseInput :: MouseInput
               }
