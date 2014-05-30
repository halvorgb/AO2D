module Model.GameState where

import Model.Camera
import Model.Light
import Model.ClearColor
import Model.Object

data GameState =
    GameState {
      gsCamera :: Camera,
      gsObjects :: [Object], -- TODO: change this to a bsp tree.
      gsLights :: [PointLight],  -- TODO: need a maxLights value or something.
      gsClearColor :: ClearColor
--      gsKeyBindings :: KeyBindings
    }
