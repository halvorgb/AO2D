module Model.State.Resources where

import Model.ShaderProgram
import Model.Texture

data Resources =
    Resources {
      rShaderPrograms :: [ShaderProgram],
      rTextures       :: [Texture]
    }

data LoadedResources =
    LoadedResources {
      lrLoadedShaderPrograms :: [LoadedShaderProgram],
      lrLoadedTextures        :: [LoadedTexture]
    } deriving (Show)
