module Model.State.Input where

data Input = Up | Down | Left | Right -- ++ more
             deriving (Eq)

type InputState = Maybe Input
