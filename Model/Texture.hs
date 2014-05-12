module Model.Texture where



data TextureResource =
    TextureResource {
      tUniqueName :: String,
      tFilePath :: FilePath
    } deriving (Show)

data Texture =
    Texture {
      ltTexture :: TextureResource,
      ltSize :: (Int, Int),
      ltPixels :: [Double]
    -- ++ more
    } deriving (Show)
