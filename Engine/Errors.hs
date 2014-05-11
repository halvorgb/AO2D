module Engine.Errors where

import Graphics.Rendering.OpenGL
import System.IO

checkError :: String -> IO ()
checkError functionName = get errors >>= mapM_ reportError
   where reportError e =
            hPutStrLn stderr (showError e ++ " detected in " ++ functionName)
         showError (Error category message) =
            "GL error " ++ show category ++ " (" ++ message ++ ")"
