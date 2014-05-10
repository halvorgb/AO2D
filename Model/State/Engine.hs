module Model.State.Engine where

import Graphics.Rendering.OpenGL

data EngineState =
    EngineState { esVertices :: [Vertex3 GLdouble],
                  esVertexColors :: [Color3 GLdouble],
                  esTime :: Double -- used for delta in mainloop
                }
