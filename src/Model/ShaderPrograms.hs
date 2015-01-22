module Model.ShaderPrograms where

import qualified Graphics.GLUtil as GLUtil

data ShaderPrograms =
    ShaderPrograms { spShadowVol :: GLUtil.ShaderProgram
                   , spLight     :: GLUtil.ShaderProgram
                   , spDepth     :: GLUtil.ShaderProgram
    }
data ShaderPrograms'Unloaded =
    ShaderPrograms'Unloaded { spShadowVolName  :: String
                            , spLightName      :: String
                            , spDepthName      :: String
                            }
