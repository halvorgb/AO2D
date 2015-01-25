{-# LANGUAGE DeriveGeneric #-}
module Model.Configuration.Model where

import           Data.Yaml
import           GHC.Generics

data ModelResource =
  ModelResource { name      :: String
                , format    :: String
                , file_name :: FilePath
                }
  deriving (Generic, Show)

instance FromJSON ModelResource
