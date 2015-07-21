module Model.Geometry where

import           Graphics.Rendering.OpenGL
import           Model.Collision

data Geometry =
    Geometry { gVertices    :: BufferObject
             , gUVCoords    :: BufferObject
             , gNormals     :: BufferObject
             , gTriElems    :: BufferObject
             , gTriAdjElems :: BufferObject
             , gNOFTris     :: GLint
             , gNOFAdjs     :: GLint
             , gVAO         :: VertexArrayObject
             , gBoundingBox :: BoundingBox
             }
