module LevelGen.Test where

import Data.Array as A
import LevelGen.LevelGen

main :: IO ()
main = do
  level <- generateLevel w h
  printLevel level
    where
      w = 80 - 1
      h = 40 - 1

printLevel :: Level -> IO ()
printLevel level =
    printLevel' min_x min_y ""
    where
      ((min_x, min_y), (max_x,max_y)) = A.bounds level

      printLevel' :: Int -> Int -> String -> IO ()
      printLevel' x y str
          | x > max_x =
              do
                putStrLn str
                printLevel' min_x (y+1) ""
          | y > max_y = putStrLn str
          | otherwise = printLevel' (x+1) y (str ++ (show $ level A.! (x,y)))
