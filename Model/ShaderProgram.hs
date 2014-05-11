module Model.ShaderProgram where

import Graphics.Rendering.OpenGL

data ShaderProgram =
    ShaderProgram {
      spUniqueName :: String,
      spVertFilePath :: FilePath,
      spFragFilePath :: FilePath
    } deriving (Show)


data LoadedShaderProgram =
    LoadedShaderProgram {
      lspShaderProgram :: ShaderProgram,
      lspProjectionMatrixIndex :: UniformLocation,
      lspColorIndex :: AttribLocation,
      lspVertexIndex :: AttribLocation
    } deriving (Show)
