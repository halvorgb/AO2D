module Model.Geometry where

import Graphics.Rendering.OpenGL

data Geometry =
    Geometry {
      gVertices :: BufferObject,
      gUVCoords :: BufferObject,
      gNormals  :: BufferObject,
      gElements :: BufferObject,
      gNOFTris  :: GLint,
      gVAO      :: VertexArrayObject
    }
