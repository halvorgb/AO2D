module Render.Setup(setupRenderer) where

import Graphics.UI.GLUT hiding ( initialize )

setupRenderer :: IO ()
setupRenderer = do
    -- Setup the basic GLUT stuff
    (_, args) <- getArgsAndInitialize
    initialWindowSize $= Size 1920 1080
    initialDisplayMode $= [ DoubleBuffered, WithDepthBuffer ]

    _window  <- createWindow "AO2D"

    -- Register the event callback functions
    displayCallback $= do
          clear [ColorBuffer, DepthBuffer]
          clearColor $= Color4 0 1 1 0
          swapBuffers
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Nothing
    idleCallback $= Nothing
    -- No need for an idle callback here, this would just hog the CPU
    -- without any visible effect

    -- At this point, control is relinquished to the GLUT event handler.
    -- Control is returned as events occur, via the callback functions.
    mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
