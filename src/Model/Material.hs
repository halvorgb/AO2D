module Model.Material where

import           Graphics.Rendering.OpenGL.GL.Texturing.Objects

data Material =
    Material { mDiffuseMap :: TextureObject
--    , mSpecularMap :: TextureObject
--      mNormalMap   :: TextureObject
             }
