module Model.State.Resources where

import Graphics.Rendering.OpenGL()

import Model.Texture
import Model.ShaderProgram
import Model.Object

data Resources =
    Resources {
      rShaderPrograms :: [ShaderProgramResource],
      rTextures       :: [TextureResource],
      rObjects        :: [ObjectResource]
    }

data LoadedResources =
    LoadedResources {
      lrShaderPrograms :: [ShaderProgram],
      lrTextures :: [Texture],
      lrObjects  :: [Object]
    } deriving (Show)
