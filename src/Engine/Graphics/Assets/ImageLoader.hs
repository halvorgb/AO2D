module Engine.Graphics.Assets.ImageLoader(loadImage) where

import           Codec.Picture
import           Codec.Picture.Types
import           System.Exit         (exitFailure)

loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage fp =
  do res <- readPng fp
     case res of
       Left err ->
         do putStrLn $ "error loading image: " ++ fp
            print err
            exitFailure
       Right rawImg -> return $ convertImg rawImg


convertImg :: DynamicImage -> Image PixelRGBA8
convertImg (ImageRGBA8 img)  = img
convertImg (ImageY8 img)     = promoteImage img
convertImg (ImageYA8 img)    = promoteImage img
convertImg (ImageRGB8 img)   = promoteImage img
convertImg _                 = error "Wrong filetype, use PNG with RGBA8"
