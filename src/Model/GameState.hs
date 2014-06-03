module Model.GameState where

import Model.Camera
import Model.Light
import Model.ClearColor
import Model.Object
import Model.Types
import Model.ShaderPrograms

data GameState =
    GameState {
      gsCamera :: Camera,
      gsObjects :: [Object], -- TODO: change this to a bsp tree.
      gsLights :: [PointLight],  -- TODO: need a maxLights value or something.
      gsClearColor :: ClearColor,
      gsAmbiance :: GLfloat, -- [0,1]
      gsShaderPrograms :: ShaderPrograms
--      gsKeyBindings :: KeyBindings
    }
