module Model.Shaders where

import Graphics.Rendering.OpenGL

data Shaders =
    Shaders { sVertFilePath :: String,
              sFragFilePath :: String
           }
