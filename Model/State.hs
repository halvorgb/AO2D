module Model.State where

import Data.IORef

import Model.State.Game
import Model.State.Input
import Model.State.Resources

type State = (IORef GameState, IORef InputState, IORef LoadedResources)
