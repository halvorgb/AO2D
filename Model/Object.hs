module Model.Object where

import Graphics.Rendering.OpenGL


data ObjectResource = ObjectModel { orUniqueName :: String,
                                    orFilePath :: FilePath
                                  }
                    | ObjectGeometry { orUniqueName :: String,
                                       orVertices :: [Vertex4 GLfloat],
                                       orElements :: [Vertex3 GLuint],
                                       orColors   :: [Vertex4 GLfloat]}

data Object =
    Object { oVertices :: BufferObject,
             oColors   :: BufferObject,
             oElements :: BufferObject,
             oNOFTris  :: Int,
             oVAO :: VertexArrayObject}
