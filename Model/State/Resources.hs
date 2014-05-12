module Model.State.Resources where

import Graphics.Rendering.OpenGL

import Model.Texture
import Model.ShaderProgram

data Resources =
    Resources {
      rShaderPrograms :: [ShaderProgramResource],
      rTextures       :: [TextureResource]
    }

data LoadedResources =
    LoadedResources {
      lrShaderPrograms :: [ShaderProgram],
      lrTextures :: [Texture]
    } deriving (Show)
