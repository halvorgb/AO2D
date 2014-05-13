module Model.Object where

import Foreign.Storable
import Foreign.Ptr

import Graphics.Rendering.OpenGL

data ObjectResource = ObjectModel { orUniqueName :: String,
                                    orFilePath :: FilePath
                                  }
                    | ObjectGeometry { orUniqueName :: String,
                                       orVertices :: [Vertex3 GLdouble]}

data Object = Object {
      oUniqueName :: String,
      oBufferObject :: BufferObject
    } deriving (Show)




data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

data ColoredVertex = ColoredVertex (Vertex4 GLfloat) (Color4 GLfloat)

instance Storable ColoredVertex where
  sizeOf ~(ColoredVertex v c) = sizeOf v + sizeOf c
  alignment ~(ColoredVertex v _) = alignment v
  peek ptr = do v <- peek (castPtr ptr)
                c <- peekByteOff (castPtr ptr) (sizeOf v)
                return $ ColoredVertex v c
  poke ptr (ColoredVertex v c) = do poke (castPtr ptr) v
                                    pokeByteOff (castPtr ptr) (sizeOf v) c
