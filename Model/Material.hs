module Model.Material where

import Graphics.Rendering.OpenGL

data MaterialResource =
    MaterialResource {
      mrUniqueName :: String,
      mrDiffuseFilePath :: FilePath
{-    trNormalFilePath  :: FilePath,
      trSpecularFilePath:: FilePath,
-}
    } deriving (Show)

data Material =
    Material {
      mUniqueName :: String,
      mSize :: (Int, Int),
      mTextureObject :: TextureObject
    -- ++ more
    } deriving (Show)
