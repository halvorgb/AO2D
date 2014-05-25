module Engine.InitEngine(initEngine) where


import Engine.Graphics.InitGraphics


import Model.State
import Model.State.Resources



initEngine :: Int -> Int -> String -> State -> Resources -> IO ()
initEngine w h winTitle state rs =
    do --initPhysics
       initGraphics w h winTitle state rs
