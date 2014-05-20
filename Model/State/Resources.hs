module Model.State.Resources where

import Model.Texture
import Model.ShaderProgram
import Model.Object

import Graphics.GLUtil
import qualified Data.Map as M

data Resources =
    Resources {
      rShaderPrograms :: [ShaderProgramResource],
      rTextures       :: [TextureResource],
      rObjects        :: [ObjectResource]
    }

data LoadedResources =
    LoadedResources {
      lrShaderPrograms :: M.Map String ShaderProgram,
      lrTextures :: [Texture], -- should be maps aswell
      lrObjects  :: M.Map String Object
    }
