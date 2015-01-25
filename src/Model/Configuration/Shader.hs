{-# LANGUAGE DeriveGeneric #-}
module Model.Configuration.Shader where

import           Data.Yaml
import           GHC.Generics

data ShaderResource =
  ShaderResource { name :: String
                 , vert :: FilePath
                 , frag :: FilePath
                 , geom :: Maybe FilePath
        }
  deriving (Generic, Show)

instance FromJSON ShaderResource
