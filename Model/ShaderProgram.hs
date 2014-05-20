module Model.ShaderProgram where

data ShaderProgramResource =
    ShaderProgramResource {
      sprUniqueName :: String,
      sprVertShader :: FilePath,
      sprFragShader :: FilePath
    }
