module Model.Geometry where

import           Graphics.Rendering.OpenGL

data Geometry =
    Geometry { gVertices    :: BufferObject
             , gUVCoords    :: BufferObject
             , gNormals     :: BufferObject
             , gTriElems    :: BufferObject
             , gTriAdjElems :: BufferObject
             , gNOFTris     :: GLint
             , gNOFAdjs     :: GLint
             , gVAO         :: VertexArrayObject
             }
