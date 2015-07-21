{-# LANGUAGE DeriveGeneric #-}
module Model.Configuration where

import           Data.Yaml
import           GHC.Generics
import           Model.Configuration.Material
import           Model.Configuration.Model
import           Model.Configuration.Shader
import           System.FilePath

data ResourceConfig =
  ResourceConfig { models    :: [ModelResource]
                 , materials :: [MaterialResource]
                 , shaders   :: [ShaderResource]
                 }
  deriving (Generic, Show)

instance FromJSON ResourceConfig

resource_config_file :: FilePath
resource_config_file = "cfg" </> "resources.cfg"

shader_file_path :: String -> FilePath
shader_file_path   fp = "assets" </> "shaders"   </> fp

material_file_path :: String -> FilePath
material_file_path fp = "assets" </> "materials" </> fp

model_file_path :: String -> FilePath
model_file_path    fp = "assets" </> "models"    </> fp
