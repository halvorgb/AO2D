module Model.ShaderPrograms where

import qualified Graphics.GLUtil as GLUtil

data ShaderPrograms =
    ShaderPrograms { spShadowVol :: GLUtil.ShaderProgram
                   , spLight     :: GLUtil.ShaderProgram
                   , spDepth     :: GLUtil.ShaderProgram
    }
data ShaderProgramsUnloaded =
    ShaderProgramsUnloaded { spShadowVolName :: String
                            , spLightName     :: String
                            , spDepthName     :: String
                            }
