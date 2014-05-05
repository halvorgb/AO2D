module LevelGen.LevelGen(generateLevel, Position, Level) where

import System.Random
import qualified Data.Array as A
import qualified Data.Array.IO as IOA
import Data.IORef

import LevelGen.Model
import LevelGen.BSP

generateLevel :: Int -> Int -> IO Level
generateLevel w h = do
  -- setup stuff
  aIO <- IOA.newArray ((0,0), (w,h)) Void :: IO (IOA.IOArray Position Tile)

  bspGen (w,h) aIO
-- pIO <- newIORef (0,0)
--  randomizeLevel pIO aIO w h


  -- convert the mutable array to an immutable one, return it.
  IOA.freeze aIO


randomizeLevel :: IORef Position -> IOA.IOArray Position Tile -> Int -> Int -> IO ()
randomizeLevel pIO aIO w h =
    do
      -- iterate through level, update pIO
      p@(x,y) <- readIORef pIO
      if x > w
      then do
        writeIORef pIO (0, y+1)
        randomizeLevel pIO aIO w h
      else if y > h
           then return ()
           else do
             writeIORef pIO (x+1, y)

             -- randomize tile
             n <- randomRIO(0, 10) :: IO Int
             let t = if n > 5
                     then Floor
                     else if n > 2
                          then Wall
                          else Void

             IOA.writeArray aIO p t

             randomizeLevel pIO aIO w h
