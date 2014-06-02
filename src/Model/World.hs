module Model.World where

import Model.InputState
import Model.GameState

import Model.Resources
import Model.Object
import Model.Entity
import Model.ShaderPrograms

import Data.IORef

type World = (GameState, IORef InputState)

type InitialState = (World, Resources, UnloadedObjects, UnloadedEntities, ShaderPrograms'Unloaded)
