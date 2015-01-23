module Model.GameState where

import           Model.Camera
import           Model.ClearColor
import           Model.Light
import           Model.Object
import           Model.ShaderPrograms
import           Model.Types

data GameState =
    GameState {
      gsCamera         :: Camera,
      gsObjects        :: [Object], -- TODO: change this to a bsp tree.
      gsLights         :: [PointLight],  -- TODO: need a maxLights value or something.
      gsClearColor     :: ClearColor,
      gsAmbiance       :: GLfloat, -- [0,1]
      gsShaderPrograms :: ShaderPrograms
--      gsKeyBindings :: KeyBindings
    }
