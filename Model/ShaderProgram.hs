module Model.ShaderProgram where

import Graphics.Rendering.OpenGL

import Lib.LoadShaders (ShaderInfo)

data ShaderProgramResource =
    ShaderProgramResource {
      sprUniqueName :: String,
      sprVertShader :: ShaderInfo,
      sprFragShader :: ShaderInfo
    }

data ShaderProgram =
    ShaderProgram {
      spUniqueName :: String,
      spProgram :: Program
    } deriving (Show)
