module Model.World where

import           Data.IORef
import           Model.Entity
import           Model.GameState
import           Model.InputState
import           Model.Object
import           Model.Resources
import           Model.ShaderPrograms

type World = (GameState, IORef InputState)

type InitialState = (World, Resources, UnloadedObjects, UnloadedEntities, ShaderProgramsUnloaded)
