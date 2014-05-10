module Engine.Setup(setupEngine) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import System.IO
import Data.IORef
import qualified Data.ByteString as BS

import Control.Monad

import System.FilePath

import Engine.InputHandler
import Engine.MainLoop

import Model.State
import Model.Shaders

-- template: https://github.com/alpmestan/glfw-b-quick-example

setupEngine :: Int -> Int -> String -> State -> IO ()
setupEngine w h winTitle state@(_, inputState) = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init

  -- load all shaders
  loadShaders state

  if not successfulInit
  then exitFailure

  else do
    mw <- GLFW.createWindow w h winTitle Nothing Nothing
    case mw of
      Nothing -> GLFW.terminate >> exitFailure
      Just window -> do
              -- window creation successful, setup callbacks
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just $ keyCallback inputState)

              -- mainLoop
              mainLoop state window

              -- mainLoop complete, exit.
              GLFW.destroyWindow window
              GLFW.terminate
              exitSuccess

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback err description = hPutStrLn stderr description


-- det her funka DÅRLIG
loadShaders :: State -> IO ()
loadShaders state = do
    ez <- loadShader ("assets" </> "shaders" </> "ez.frag") FragmentShader
    print ez
    frag <- loadShader ("assets" </> "shaders" </> "lol.frag") FragmentShader
    vert <- loadShader ("assets" </> "shaders" </> "lol.vert") VertexShader
    print vert
    print frag

loadShader :: String -> ShaderType -> IO (Shader)
loadShader filePath shaderType = do
  source <- BS.readFile filePath

  print source
  shader <- createShader shaderType
  shaderSourceBS shader $= source
  compileShader shader
  printError

  ok <- get $ compileStatus shader
  infoLog <- get $ shaderInfoLog shader

  unless (null infoLog)
             (mapM_ putStrLn
              ["Shader info log for '" ++ filePath ++ "':", infoLog, ""])

  print ok
  if not ok
  then do deleteObjectNames [shader]
          error ("shader compilation failed on: " ++ filePath)
  else return shader
{-
loadShaderBS :: FilePath -> ShaderType -> BS.ByteString -> IO Shader
loadShaderBS filePath st src = do
  shader <- createShader st
  shaderSourceBS shader $= src
  compileShader shader
  printError
  ok <- get (compileStatus shader)
  infoLog <- get (shaderInfoLog shader)
  unless (null infoLog)
         (mapM_ putStrLn
                ["Shader info log for '" ++ filePath ++ "':", infoLog, ""])
  unless ok $ do
    deleteObjectNames [shader]
    ioError (userError "shader compilation failed")
  return shader
-}
printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)
