module Model.Object where

import Foreign.Storable
import Foreign.Ptr

import Graphics.Rendering.OpenGL
import qualified Linear as L

data ObjectResource = ObjectModel { orUniqueName :: String,
                                    orFilePath :: FilePath
                                  }
                    | ObjectGeometry { orUniqueName :: String,
                                       orVertices :: [L.V3 Float],
                                       orElements :: [L.V3 GLuint],
                                       orColors   :: [L.V3 Float]}

data Object =
    Object { oVertices :: BufferObject,
             oColors   :: BufferObject,
             oElements :: BufferObject,
             oNOFTris  :: Int}

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
