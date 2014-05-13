module Model.ShaderProgram where

import Graphics.Rendering.OpenGL

data ShaderProgramResource =
    ShaderProgramResource {
      sprUniqueName :: String,
      sprVertShader :: FilePath,
      sprFragShader :: FilePath
    }
