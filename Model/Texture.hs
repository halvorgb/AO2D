module Model.Texture where



data Texture =
    Texture {
      tFilePath :: FilePath
    } deriving (Show)

data LoadedTexture =
    LoadedTexture {
      ltTexture :: Texture,
      ltSize :: (Int, Int),
      ltPixels :: [Double]
    -- ++ more
    } deriving (Show)
