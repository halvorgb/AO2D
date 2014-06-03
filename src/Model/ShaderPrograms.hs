module Model.ShaderPrograms where

import qualified Graphics.GLUtil as GLUtil

data ShaderPrograms =
    ShaderPrograms {
      spDepth     :: GLUtil.ShaderProgram,
      spShadowVol :: GLUtil.ShaderProgram,
      spLight     :: GLUtil.ShaderProgram
    }
data ShaderPrograms'Unloaded =
    ShaderPrograms'Unloaded {
      spDepthName :: String,
      spShadowVolName :: String,
      spLightName :: String
    }
