module Model.State.Input where

data Input = Up | Down | Left | Right -- ++ more
             deriving (Eq, Show, Ord)

type InputState = Maybe Input
