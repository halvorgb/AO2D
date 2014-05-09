module LevelGen.BSP(bspGen) where

import Data.IORef
import qualified Data.Array as A
import qualified Data.Array.IO as IOA
import System.Random
import qualified Data.List as L
import LevelGen.Model


data Direction = Horizontal | Vertical deriving(Eq, Show)
data BSPTree = Node { nLeftChild :: BSPTree,
                      nRightChild :: BSPTree
                    }

             | Room { rPosition :: (Int,Int),
                      rSize :: (Int, Int)
                    }
               deriving (Show, Eq)

-- Constants:

smallestRoomDimension = 6

smallestBSPNodeSize = 12
largestBSPNodeSize = 18

largeRoomChance = 23 -- percent


-- public functions:
-- bspGen takes in a level filled with walls, carves out rooms
bspGen :: (Int, Int) -> IOA.IOArray Position Tile -> IO ()
bspGen (w,h) level = do
  g <- newStdGen

  let bspTree = spawnRoomTree (w,h) (0,0) g
  centreList <- newIORef [] :: IO (IORef [(Int, Int)])
  buildRooms bspTree level centreList
  centreList' <- readIORef centreList

  connectRooms level centreList' []

  IOA.writeArray level (head centreList') Liquid
  IOA.writeArray level (last centreList') Stairs

-- private functions

-- connects two and two rooms, for each list member, connect to the nearest room.
-- TODO: Prevent clustering.
connectRooms :: IOA.IOArray Position Tile -> [(Int, Int)] -> [(Int, Int)] -> IO ()
connectRooms _ [] _ = return () -- last iteration
connectRooms level (c:cs) [] = connectRooms level cs [c] -- first iteration
connectRooms level centres@(c:cs) visitedCentres
             = do writeHallway c1 c2
                  connectRooms level cs (c:visitedCentres)
    where
      -- find the pair of centres/visitedCentres such that the distance is minimal.
      (c1, c2,_) = L.foldl' (\acc@(c1, c2, d) cp@(c1',c2',d')  -> if d' < d then cp else acc) ((0,0), (0,0), maxBound :: Int)
                   [(c1, c2, dist c1 c2) | c1 <- centres, c2 <- visitedCentres]

      dist (x1,y1) (x2, y2) =
          (abs x2-x1) + (abs y2-y1)

      writeHallway :: (Int, Int) -> (Int, Int) -> IO ()
      writeHallway c1'@(x1, y1) c2'@(x2, y2)
          | c1' == c2' = return () -- done
          | otherwise = do let border = [(x1-1, y1-1), -- topleft
                                         (x1  , y1-1), -- top
                                         (x1+1, y1-1), -- topright
                                         (x1-1, y1  ), -- left
                                         (x1+1, y1  ), -- right
                                         (x1-1, y1+1), -- bottomleft
                                         (x1  , y1+1), -- bottom
                                         (x1+1, y1+1)] -- bottomleft

                           IOA.writeArray level c1' Floor
                           mapM_ replaceVoid border

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


-- coordinates here include walls, so if w*h = size, then the room-floor cant be of size w*h
buildRooms :: BSPTree -> IOA.IOArray Position Tile -> IORef [(Int, Int)] -> IO ()
buildRooms (Room (x,y) (w,h)) level clIO=
    do g <- newStdGen
       let (x', gx) = randomR (x, x+w-smallestRoomDimension) g :: (Int, StdGen)
           (w', gw) = randomR (smallestRoomDimension, w-(x'-x)) gx :: (Int, StdGen)
           (y', gy) = randomR (y, y+h-smallestRoomDimension) gw :: (Int, StdGen)
           (h', gh) = randomR (smallestRoomDimension, h-(y'-y)) gy :: (Int, StdGen)

           -- // what to draw
           interior_indices = [(x'', y'') | x'' <- [x'+1..x'+w'-1], y'' <- [y'+1..y'+h'-1]]

           border_indices   = [(x'', y')    | x'' <- [x'..x'+w']] ++ -- top w/corners
                              [(x'', y'+h') | x'' <- [x'..x'+w']] ++ -- bottom w/corners
                              [(x' ,   y'') | y'' <- [y'+1..y'+h'-1]] ++   -- left
                              [(x'+w', y'') | y'' <- [y'+1..y'+h'-1]]      -- right

           -- // where centre is (to draw hallways inbetween nodes)
           max_centre_noise_x = (w' `div` 2) - 1
           (centre_noise_x, gcnx) = randomR (-max_centre_noise_x,max_centre_noise_x) gh :: (Int, StdGen)
           max_centre_noise_y = (h' `div` 2) - 1
           (centre_noise_y, _) = randomR (-max_centre_noise_y,max_centre_noise_y) gcnx :: (Int, StdGen)
           centre_x = centre_noise_x + x' + (w' `div` 2)
           centre_y = centre_noise_y + y' + (h' `div` 2)

       -- change array at indices
       mapM_ (\pos -> IOA.writeArray level pos Floor) interior_indices
       mapM_ (\pos -> IOA.writeArray level pos Wall) border_indices

       -- change centrelist
       cl <- readIORef clIO
       writeIORef clIO ((centre_x, centre_y):cl)


-- recurse
buildRooms (Node l r) level clIO = do buildRooms l level clIO
                                      buildRooms r level clIO

-- spawnChildren builds the BSPTree top down. The undefined values are always written before read.
spawnRoomTree :: (Int,Int) -> (Int, Int) -> StdGen -> BSPTree
spawnRoomTree (w,h) (x,y) g
    | createRoom = Room { rPosition = (x,y),
                          rSize     = (w, h)
                        }
    | otherwise = Node { nLeftChild = spawnRoomTree sl pl gl,
                         nRightChild = spawnRoomTree sr pr gr
                       }
    where

      -- Decide whether to create a room or to split further:
      (largeRoomRoll, g_lrr) = randomR (0,100) g :: (Int, StdGen)
      createLargeRoom = w < largestBSPNodeSize &&
                        h < largestBSPNodeSize &&
                        largeRoomRoll < largeRoomChance

      createSmallRoom = w < smallestBSPNodeSize*2 &&
                        h < smallestBSPNodeSize*2


      createRoom = createSmallRoom ||
                   createLargeRoom


      -- for splitting into further BSPTrees:
      (horizontal_splitRoll, g_hs) = randomR (0,1) g_lrr :: (Int, StdGen)
      horizontal_split
          | w < smallestBSPNodeSize*2 = True
          | h < smallestBSPNodeSize*2 = False
          | otherwise = horizontal_splitRoll == 0

      (cut, g_c)
          | horizontal_split = -- horiz
              randomR (smallestBSPNodeSize, h - smallestBSPNodeSize) g_hs
          | otherwise = -- vertical
              randomR (smallestBSPNodeSize, w - smallestBSPNodeSize) g_hs

      (sl, pl, sr, pr)
          | horizontal_split = -- horiz.
              ((w, cut    ), (x, y),  -- l
               (w, h-cut), (x, y + cut))  -- r

          | otherwise = -- vertical
              ((cut, h    ), (x, y),  -- l
               (w-cut, h), (x + cut, y))  -- r

      (gl, gr) = split g_c
