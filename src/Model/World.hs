module Model.World where

import           Data.IORef
import           Model.Entity
import           Model.GameState
import           Model.InputState
import           Model.Object

type World = (GameState, IORef InputState)

type InitialState = (World, UnloadedObjects, UnloadedEntities)
