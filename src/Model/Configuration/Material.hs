{-# LANGUAGE DeriveGeneric #-}
module Model.Configuration.Material where

import           Data.Yaml
import           GHC.Generics

data MaterialResource =
  MaterialResource { name    :: String
                   , diffuse :: FilePath
                     --           , normal   :: Maybe FilePath
                     --           , specular :: Maybe FilePath
                   }
  deriving (Generic, Show)

instance FromJSON MaterialResource
