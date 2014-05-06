module LevelGen.BSP(bspGen) where

import Data.IORef
import qualified Data.Array as A
import qualified Data.Array.IO as IOA
import System.Random

import LevelGen.Model


data Direction = Horizontal | Vertical deriving(Eq, Show)
data BSPTree = Node { nSize :: (Int,Int),
                      nPosition :: (Int, Int),
                      nLeftChild :: BSPTree,
                      nRightChild :: BSPTree
                    }

             | Leaf
               deriving (Show, Eq)

-- Constants:

smallestRoomDimension = 4

smallestBSPNodeSize = 8
largestBSPNodeSize = 19
largestBSPNodeOffset = 4
largeRoomChance = 23 -- percent


-- public functions:
-- bspGen takes in a level filled with walls, carves out rooms
bspGen :: (Int, Int) -> IOA.IOArray Position Tile -> IO ()
bspGen (w,h) level = do
  g <- newStdGen
  let bspTree = spawnChildren mother g
  buildRooms bspTree level

  centreList <- newIORef [] :: IO (IORef [(Int, Int)])
  buildCentreList bspTree centreList
  centreList' <- readIORef centreList
--  print centreList'
  connectRooms bspTree level centreList'

  IOA.writeArray level (head centreList') Liquid
  IOA.writeArray level (last centreList') Stairs

    where
      mother = Node {nSize = (w,h),
                     nPosition = (0,0),
                     nLeftChild = undefined,
                     nRightChild = undefined}

-- private functions


-- connects two and two rooms, children of the same node.
connectRooms :: BSPTree -> IOA.IOArray Position Tile -> [(Int, Int)] -> IO ()
connectRooms tree level [c] = return ()
connectRooms tree level (c1:c2:cs) = do writeHallway c1 c2
                                        connectRooms tree level (c2:cs)
    where
      writeHallway :: (Int, Int) -> (Int, Int) -> IO ()
      writeHallway c1'@(x1, y1) c2'@(x2, y2)
          | c1' == c2' = return () -- done
          | otherwise = do let left  = (x1-1, y1  )
                               right = (x1+1, y1  )
                               top   = (x1  , y1-1)
                               bot   = (x1  , y1+1)

                           IOA.writeArray level c1' Floor
                           mapM_ replaceVoid [left,right,top,bot]

                           next

          where
            replaceVoid :: (Int, Int) -> IO ()
            replaceVoid pos = do t <- IOA.readArray level pos
                                 if t == Void
                                 then IOA.writeArray level pos Wall
                                 else return ()

            next
                | x2 > x1 = writeHallway (x1+1, y1) c2
                | x2 < x1 = writeHallway (x1-1, y1) c2
                | y2 > y1 = writeHallway (x1, y1+1) c2
                | y2 < y1 = writeHallway (x1, y1-1) c2
                | otherwise = error "connectRooms: WAT."

-- traverses the tree, adds every centre location to the IORef list
buildCentreList :: BSPTree -> IORef [(Int, Int)] -> IO ()
buildCentreList tree clIO
    | l == Leaf &&
      r == Leaf = do g <- newStdGen

                     let (x,y) = nPosition tree
                         (w,h) = nSize tree
                         (centreNoise, _) = randomR (-2,2) g :: (Int, StdGen)
                         centre@(cx, cy) = (centreNoise + x + w `div` 2, centreNoise + y + h `div` 2)

                     cl <- readIORef clIO
                     writeIORef clIO (centre:cl)


    | otherwise = do buildCentreList l clIO
                     buildCentreList r clIO
      where
        l = nLeftChild tree
        r = nRightChild tree
-- fills the interior tiles and wall tiles coresponding to leaf nodes.
buildRooms :: BSPTree -> IOA.IOArray Position Tile -> IO ()
buildRooms tree level
    | l == Leaf &&
      r == Leaf = do g <- newStdGen
                     let (x,y) = nPosition tree
                         (w,h) = nSize tree
                         maxPadding = max 1 (((min w h) - smallestRoomDimension) `div` 2)
                         (padding_low, g') = randomR (1, maxPadding) g :: (Int, StdGen)
                         (padding, _) = randomR (padding_low, maxPadding) g' :: (Int, StdGen) -- try to roll higher

                         interior_indices = [(x', y') | x' <- [x+padding..x+w-padding], y' <- [y+padding..y+h-padding]]

                         border_indices   = [(x', y+padding-1)   | x' <- [x+padding-1..x+w-padding+1]] ++ -- top w/corners
                                            [(x', y+h-padding+1) | x' <- [x+padding-1..x+w-padding+1]] ++ -- bottom w/corners
                                            [(x+padding-1,   y') | y' <- [y+padding..y+h-padding]] ++   -- left
                                            [(x+w-padding+1, y') | y' <- [y+padding..y+h-padding]]      -- right

                     -- change array at indices
                     mapM_ (\pos -> IOA.writeArray level pos Floor) interior_indices
                     mapM_ (\pos -> IOA.writeArray level pos Wall) border_indices


    | otherwise = do buildRooms l level
                     buildRooms r level

      where
        l = nLeftChild tree
        r = nRightChild tree



-- spawnChildren builds the BSPTree top down. The undefined values are always written before read.
spawnChildren :: BSPTree -> StdGen -> BSPTree
spawnChildren mother g = mother {nLeftChild = l,
                                 nRightChild = r
                                }
    where
      (w,h) = nSize mother
      (x,y) = nPosition mother

      -- random direction
      (d, g') = random g :: (Bool, StdGen)
      dir :: Direction
      dir
          | d     = Horizontal
          | not d = Vertical

      -- whether to make a big room or not:
      (largeRoomRoll, g'') = randomR (0, 100) g :: (Int, StdGen)
      largeRoom = largeRoomRoll < largeRoomChance &&
                  h < largestBSPNodeSize &&
                    w < largestBSPNodeSize




      (oX, g''') = randomR (0, largestBSPNodeOffset) g'' :: (Int, StdGen)
      (oY, g'''') = randomR (0, largestBSPNodeOffset) g''' :: (Int, StdGen)

      (x', y') = (x+oX, y+oY)
      (w', h') = (w-oX, h-oY)

      (l,r) = generateChildren dir


      generateChildren :: Direction -> (BSPTree, BSPTree)
      generateChildren d
          | largeRoom = (Leaf, Leaf) -- Make a Large Room
          | d == Horizontal =
              if h' > 2*smallestBSPNodeSize
              then let (cutY, cg) = randomR (smallestBSPNodeSize, h' - smallestBSPNodeSize) g'''
                       lm = Node { nSize = (w', cutY),
                                   nPosition = (x', y'),
                                   nLeftChild = undefined,
                                   nRightChild = undefined
                                 }
                       rm = lm { nSize = (w', h'- cutY),
                                 nPosition = (x', y' +cutY)
                               }
                       (gl,gr) = split cg
                   in (spawnChildren lm gl, spawnChildren rm gr) -- Divide room horizontally

              else if w' >= largestBSPNodeSize
                   then generateChildren Vertical -- Divide the other way
                   else (Leaf, Leaf) -- Make a small room (no division possible)

          | d == Vertical =
              if w' > 2*smallestBSPNodeSize
              then let (cutX, cg) = randomR (smallestBSPNodeSize, w' - smallestBSPNodeSize) g'''
                       lm = Node { nSize = (cutX, h'),
                                   nPosition = (x', y'),
                                   nLeftChild = undefined,
                                   nRightChild = undefined
                                 }
                       rm = lm { nSize = (w' - cutX, h'),
                                 nPosition = (x' + cutX, y')
                               }
                       (gl,gr) = split cg
                   in (spawnChildren lm gl, spawnChildren rm gr) -- Divide room vertically
              else if h' >= largestBSPNodeSize
                   then generateChildren Horizontal -- Divide the other way
                   else (Leaf, Leaf) -- Make a small room (no division possible)
