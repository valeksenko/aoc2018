module D23P2 (
    shortestdistance
) where

import Data.List
import Data.Ord
import Data.Function
import Debug.Trace
import Data.Tuple.Extra

type Coordinate = (Int, Int, Int)
type Radius = Int

shortestdistance :: Int -> [(Coordinate, Radius)] -> Int
shortestdistance groupSize positions = let
        inRange p1 (p2, radius) = radius >= distance p1 p2
        closeCnt p = (p, length $ filter (inRange p) positions)
        closest = fst . minimumBy (comparing $ distance (0,0,0) . fst)
        closestGroup = head . groupBy ((==) `on` snd) . sortBy (comparing snd)
    in distance (0,0,0) . closest . closestGroup . map closeCnt $ clusterPositions groupSize positions

clusterPositions :: Int -> [(Coordinate, Radius)] -> [Coordinate]
clusterPositions groupSize positions = let
        surroundPositions ((x,y,z), r) = [(x', y', z') | x' <- [x-r..x+r], y' <- [y-r..y+r], z' <- [z-r..z+r]]
        debug l = traceShow (length l) l
        inRange n@(p, r) = (n, length . filter ((<=) r) $ map (distance p . fst) positions)
        largestGroup = map fst . concat . take groupSize . groupBy ((==) `on` snd) . sortBy (flip $ comparing snd)
    in debug . concat . map surroundPositions . debug . largestGroup $ map inRange positions

distance :: Coordinate -> Coordinate -> Int
distance (x1, y1, z1) (x2, y2, z2) = let
        calcD a b = abs $ a - b
    in calcD x1 x2 + calcD y1 y2 + calcD z1 z2

{-
https://adventofcode.com/2018/day/23#part2

Now, you just need to figure out where to position yourself so that you're actually teleported when the nanobots activate.

To increase the probability of success, you need to find the coordinate which puts you in range of the largest number of nanobots. If there are multiple, choose one closest to your position (0,0,0, measured by manhattan distance).

For example, given the following nanobot formation:

pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5
Many coordinates are in range of some of the nanobots in this formation. However, only the coordinate 12,12,12 is in range of the most nanobots: it is in range of the first five, but is not in range of the nanobot at 10,10,10. (All other coordinates are in range of fewer than five nanobots.) This coordinate's distance from 0,0,0 is 36.

Find the coordinates that are in range of the largest number of nanobots. What is the shortest manhattan distance between any of those points and 0,0,0?
-}