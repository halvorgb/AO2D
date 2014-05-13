module Model.Texture where



data TextureResource =
    TextureResource {
      trUniqueName :: String,
      trFilePath :: FilePath
    } deriving (Show)

data Texture =
    Texture {
      tUniqueName :: String,
      tSize :: (Int, Int),
      tPixels :: [Double]
    -- ++ more
    } deriving (Show)
