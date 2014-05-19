module Model.State.Input where

data Input = Up | Down | Left | Right | Forward | Backward -- ++ more
             deriving (Eq, Show, Ord)

type InputState = [Input]
