module Model.State where

import Data.IORef

import Model.State.Game
import Model.State.Input

type State = (IORef GameState, IORef InputState)
