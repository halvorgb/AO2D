module Model.Level where

import qualified Data.Array as A

import Model.Common


type Level = A.Array Position Tile

data Tile = Wall | Floor | Void | Liquid | Stairs
          deriving(Eq)

instance Show Tile where
    show t
        | t == Wall   = "#"
        | t == Floor  = "."
        | t == Void   = " "
        | t == Liquid = "~"
        | t == Stairs = "\\"
        | otherwise   = error "Unrecognized tile in show instance."
