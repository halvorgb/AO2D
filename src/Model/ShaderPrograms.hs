module Model.ShaderPrograms where

import qualified Graphics.GLUtil as GLUtil

data ShaderPrograms =
    ShaderPrograms {
      spSilhouette :: GLUtil.ShaderProgram,
      spLight :: GLUtil.ShaderProgram
    }
data ShaderPrograms'Unloaded =
    ShaderPrograms'Unloaded {
      spSilhouetteName :: String,
      spLightName :: String
    }
