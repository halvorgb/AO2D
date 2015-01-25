module Model.GameState where

import           Model.Camera
import           Model.ClearColor
import           Model.Light
import           Model.Object
import           Model.Types
import qualified Graphics.GLUtil                    as GLUtil
import qualified Data.Map                           as M

data GameState =
    GameState { gsCamera         :: Camera
              , gsObjects        :: [Object] -- TODO: change this to a bsp tree.
              , gsLights         :: [PointLight]  -- TODO: need a maxLights value or something.
              , gsClearColor     :: ClearColor
              , gsAmbiance       :: GLfloat -- [0,1]
              , gsShaderPrograms :: M.Map String GLUtil.ShaderProgram
                -- , gsKeyBindings :: KeyBindings
              }

getLightShader :: GameState -> GLUtil.ShaderProgram
getLightShader = getShader "light"

getDepthShader :: GameState -> GLUtil.ShaderProgram
getDepthShader = getShader "depth"

getShadowShader :: GameState -> GLUtil.ShaderProgram
getShadowShader = getShader "shadow"

getShader :: String -> GameState -> GLUtil.ShaderProgram
getShader str gs = (gsShaderPrograms gs) M.! str
