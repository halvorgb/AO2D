module Engine.Shaders(loadShader, destroyAllShaders) where

import Graphics.Rendering.OpenGL
import Control.Monad
import qualified Data.ByteString as B
import System.IO
import Data.IORef

import Engine.Errors

import Model.State
import Model.State.Resources
import Model.ShaderProgram

-- https://github.com/haskell-opengl/GLUT/blob/master/examples/Misc/SmoothOpenGL3.hs
-- ^ how to load shaders from there.

destroyAllShaders :: State -> IO ()
destroyAllShaders st = return ()

loadShader :: IORef LoadedResources -> ShaderProgram -> IO ()
loadShader st sp = do
  vertFile <- B.readFile $ spVertFilePath sp
  fragFile <- B.readFile $ spFragFilePath sp

  vertShader <- compileShaderSource VertexShader vertFile
  fragShader <- compileShaderSource VertexShader fragFile
  program <- createProgramUsing [vertShader, fragShader]

  currentProgram $= Just program  -- <-  this isn't needed before drawing?

  projectionMatrixIndex <- get $ uniformLocation program "fg_ProjectionMatrix"

  colorIndex <- get $ attribLocation program "fg_Color"
  vertexAttribArray colorIndex $= Enabled

  vertexIndex <- get $ attribLocation program "fg_Vertex"
  vertexAttribArray vertexIndex $= Enabled

  checkError "initShader"

  let newLSP =
          LoadedShaderProgram {
                     lspShaderProgram = sp,
                     lspProjectionMatrixIndex = projectionMatrixIndex,
                     lspColorIndex = colorIndex,
                     lspVertexIndex = vertexIndex
                   }

  print newLSP

--  // add the new LSP to the resoureState IORef
  modifyIORef st $ (\lrs -> lrs {
                              lrLoadedShaderPrograms = newLSP:(lrLoadedShaderPrograms lrs)
                            })


-- compile shader
compileShaderSource :: ShaderType -> B.ByteString -> IO Shader
compileShaderSource st source = do
   shader <- createShader st
   shaderSourceBS shader $= source
   compileAndCheck shader
   return shader

-- compile shader (contd.), check for errors
compileAndCheck :: Shader -> IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

-- error checking helper
checked :: (t -> IO ()) -> (t -> GettableStateVar Bool) -> (t -> GettableStateVar String) -> String -> t -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   status <- get (getStatus object)
   unless status $
      hPutStrLn stderr . ((message ++ " log: ") ++) =<< get (getInfoLog object)


-- create shader Program, called in loadShaders
createProgramUsing :: [Shader] -> IO Program
createProgramUsing shaders = do
   program <- createProgram
   attachedShaders program $= shaders
   linkAndCheck program
   return program

-- hlper for createProgramUsing
linkAndCheck :: Program -> IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"
