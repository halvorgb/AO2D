module Engine.Graphics.Common(errorCallback, checkError, resizeCallback, dumpInfo) where

import           Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW          as GLFW
import           System.IO

checkError :: String -> IO ()
checkError functionName = get errors >>= mapM_ reportError
  where reportError e =
          hPutStrLn stderr (showError e ++ " detected in " ++ functionName)
        showError (Error category message) =
          "GL error " ++ show category ++ " (" ++ message ++ ")"

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

resizeCallback :: GLFW.WindowSizeCallback
resizeCallback _ width height =
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

dumpInfo :: IO ()
dumpInfo = do
  dump "Vendor  " vendor
  dump "Renderer" renderer
  dump "Version " glVersion
  dump "GLSL    " shadingLanguageVersion
  checkError "dumpInfo"
  where dump :: String -> GettableStateVar String -> IO ()
        dump message var = putStrLn . ((message ++ ": ") ++) =<< get var
