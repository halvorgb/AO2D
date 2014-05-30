module Model.Resources where

data Resources =
    Resources {
      rGeometryRs :: [GeometryResource],
      rShaderRs :: [ShaderResource],
      rMaterialRs :: [MaterialResource]
    }

data ShaderResource =
    ShaderResource {
      srUniqueName :: String,
      srVertShaderFP :: FilePath,
      srGeomShaderFP :: FilePath,
      srFragShaderFP :: FilePath
}


data ModelFormat = ModelFormat'OBJ
                   deriving(Eq, Show)

data GeometryResource =
    GeometryResource {
      grUniqueName :: String,
      grModelFormat :: ModelFormat,
      grModelFP :: FilePath
    } deriving (Show)


data MaterialResource =
    MaterialResource {
      mrUniqueName :: String,
      mrDiffuseFP  :: FilePath,
      mrSpecularFP :: FilePath,
      mrNormalFP   :: FilePath
    }
