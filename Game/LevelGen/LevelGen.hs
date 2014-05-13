module Game.LevelGen.LevelGen(generateLevel) where

import System.Random
import qualified Data.Array as A
import qualified Data.Array.IO as IOA
import Data.IORef
import Control.Monad

import Game.LevelGen.BSP

import Model.Level
import Model.Common


generateLevel :: Int -> Int -> IO Level
generateLevel w h = do
  -- setup stuff
  aIO <- IOA.newArray ((0,0), (w',h')) Void :: IO (IOA.IOArray Position Tile)

  bspGen (w',h') aIO

  -- convert the mutable array to an immutable one, return it.
  IOA.freeze aIO
    where
      (w',h') = (w-1, h-1)


randomizeLevel :: IORef Position -> IOA.IOArray Position Tile -> Int -> Int -> IO ()
randomizeLevel pIO aIO w h =
    do
      -- iterate through level, update pIO
      p@(x,y) <- readIORef pIO
      if x > w
      then do
        writeIORef pIO (0, y+1)
        randomizeLevel pIO aIO w h
      else unless (y > h) $ do
        writeIORef pIO (x+1, y)

        -- randomize tile
        n <- randomRIO(0, 10) :: IO Int
        let t | n > 5 = Floor
              | n > 2 =  Wall
              | otherwise = Void

        IOA.writeArray aIO p t

        randomizeLevel pIO aIO w h
