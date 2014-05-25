module Model.Object where

import Graphics.Rendering.OpenGL


data ModelFormat = ModelFormat'OBJ
                 deriving(Eq, Show)

data ObjectResource =
    ObjectResource { omrUniqueName :: String,
                     omrFilePath :: FilePath,
                     omrModelFormat :: ModelFormat
                   } deriving(Show)

data Object =
    Object { oVertices :: BufferObject,
             oUV       :: BufferObject,
             oNormals  :: BufferObject,
             oElements :: BufferObject,
             oNOFTris  :: Int,
             oVAO :: VertexArrayObject}
