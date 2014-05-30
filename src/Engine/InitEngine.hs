module Engine.InitEngine(initEngine) where


import Engine.Graphics.InitGraphics

import Model.World


initEngine :: Int -> Int -> String -> InitialState -> IO ()
initEngine w h winTitle initialState  =
    do --initPhysics
       initGraphics w h winTitle initialState
