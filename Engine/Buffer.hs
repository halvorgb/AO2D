module Engine.Buffer(initBuffer) where

import Data.IORef

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr

import Graphics.Rendering.OpenGL

import Engine.Errors

import Model.State.Resources
import Model.Object






bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initBuffer :: IO Descriptor
initBuffer = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vertices = [
        -- Triangle 1
        ColoredVertex (Vertex4 (-0.90) (-0.90) (-0.1) 1) (Color4 1 0 0 1),
        ColoredVertex (Vertex4   0.85  (-0.90) (0.3)  1) (Color4 0 1 0 1),
        ColoredVertex (Vertex4 (-0.90)   0.85  (0.9)  1) (Color4 0 0 1 1),
        -- Triangle 2
        ColoredVertex (Vertex4   1 0 0 1) (Color4 0 1 1 1),
        ColoredVertex (Vertex4   0 1 0 1) (Color4 1 0 1 1),
        ColoredVertex (Vertex4   0 0 1 1) (Color4 1 1 0 1)]
      numVertices = length vertices
      vertexSize = sizeOf (head vertices)

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let firstIndex = 0
      vPosition = AttribLocation 0
      vColor = AttribLocation 1
  vertexAttribPointer vPosition $=
    (ToFloat,
     VertexArrayDescriptor 4 Float (fromIntegral vertexSize)
                           (bufferOffset (firstIndex * vertexSize)))
  vertexAttribArray vPosition $= Enabled
  let colorOffset = case head vertices of ~(ColoredVertex v _) -> sizeOf v
  vertexAttribPointer vColor $=
    (ToFloat,
     VertexArrayDescriptor 4 Float (fromIntegral vertexSize)
                           (bufferOffset ((firstIndex * vertexSize) +
                                          fromIntegral colorOffset)))
  vertexAttribArray vColor $= Enabled

  return $
    Descriptor triangles (fromIntegral firstIndex) (fromIntegral numVertices)













{-


initBuffer :: IORef LoadedResources -> ObjectResource -> IO ()
initBuffer resState resToLoad = do
  bufferObject <- genObjectName
  bindBuffer ArrayBuffer $= Just bufferObject
  withArray varray $ \buffer ->
      bufferData ArrayBuffer $= (fromIntegral sizeOfVarray, buffer, StaticDraw)

  checkError "initBuffer"

  let newObject =
          Object "test" bufferObject

  modifyIORef resState
         (\ldRs -> let loadedObjects =lrObjects ldRs
                   in ldRs {lrObjects = newObject:loadedObjects})


varray :: [GLfloat]
varray = [
   1, 0, 0, -- red
   5, 5,    -- lower left

   0, 1, 0, -- green
   25, 5,   -- lower right

   0, 0, 1, -- blue
   5, 25 ]  -- upper left

sizeOfComponent :: Int
sizeOfComponent = sizeOf (head varray)

--stride :: Stride
--gstaride = fromIntegral sizeOfComponent * fromIntegral (numColorComponents + numVertexComponents)

sizeOfVarray :: Int
sizeOfVarray = length varray * sizeOfComponent
-}
