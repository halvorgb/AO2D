module Model.State.Resources where


import Model.Material

import Model.ShaderProgram
import Model.Object

import Graphics.GLUtil
import qualified Data.Map as M

data Resources =
    Resources {
      rShaderPrograms :: [ShaderProgramResource],
      rMaterials       :: [MaterialResource],
      rObjects        :: [ObjectResource]
    }

data LoadedResources =
    LoadedResources {
      lrShaderPrograms :: M.Map String ShaderProgram,
      lrMaterials :: M.Map String Material,
      lrObjects  :: M.Map String Object
    }
