module D6P1 (
  largestarea
) where

import Data.List
import Data.Ord
import Data.Function

type Coord = (Int, Int)

data Point =
    Point {
        pCoord :: Coord
      , pSeed  :: Coord
    } deriving(Show, Eq)

data Grid =
    Grid {
        gSeeds  :: [Coord]
      , gPoints :: [Point]
      , minX  :: Int
      , maxX   :: Int
      , minY  :: Int
      , maxY   :: Int
    } deriving(Show, Eq)

largestarea :: [Coord] -> Int
largestarea = length . largestA . newGrid
    where
        largestA g = last . sortBy (comparing length) . group . sort $ nonEdges g

newGrid :: [Coord] -> Grid
newGrid seeds = Grid seeds points minX maxX minY maxY
    where
        minX   = fst $ minimumBy (comparing fst) seeds
        maxX   = fst $ maximumBy (comparing fst) seeds
        minY   = snd $ minimumBy (comparing snd) seeds
        maxY   = snd $ maximumBy (comparing snd) seeds
        points = gridPoints seeds minX maxX minY maxY

gridPoints :: [Coord] -> Int -> Int -> Int -> Int -> [Point]
gridPoints seeds minX maxX minY maxY = foldr point [] [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
    where
        point p a        = shortest p a $ shortests p
        shortest p a [e] = (Point p (snd e)):a
        shortest _ a _   = a
        shortests p      = head . groupBy ((==) `on` fst) . sortBy (comparing fst) $ map (sDistance p) seeds
        sDistance p s    = ((distance fst p s) + (distance snd p s), s)
        distance f a b   = abs $ (f a) - (f b)

nonEdges :: Grid -> [Coord]
nonEdges g = filter (limitedSeed g) $ map pSeed (gPoints g)
    where
        limitedSeed g s = notElem s seedEdges
        seedEdges = map head . group . sort . map pSeed . filter edge $ gPoints g
        edge p = ((fst $ pCoord p) == (minX g))
                    || ((fst $ pCoord p) == (maxX g))
                    || ((snd $ pCoord p) == (minY g))
                    || ((snd $ pCoord p) == (maxY g))

{-
https://adventofcode.com/2018/day/6

Using only the Manhattan distance, determine the area around each coordinate by counting the number of integer X,Y locations that are closest to that coordinate (and aren't tied in distance to any other coordinate).

Your goal is to find the size of the largest area that isn't infinite. For example, consider the following list of coordinates:

1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
If we name these coordinates A through F, we can draw them on a grid, putting 0,0 at the top left:

..........
.A........
..........
........C.
...D......
.....E....
.B........
..........
..........
........F.
This view is partial - the actual grid extends infinitely in all directions. Using the Manhattan distance, each location's closest coordinate can be determined, shown here in lowercase:

aaaaa.cccc
aAaaa.cccc
aaaddecccc
aadddeccCc
..dDdeeccc
bb.deEeecc
bBb.eeee..
bbb.eeefff
bbb.eeffff
bbb.ffffFf
Locations shown as . are equally far from two or more coordinates, and so they don't count as being closest to any.

In this example, the areas of coordinates A, B, C, and F are infinite - while not shown here, their areas extend forever outside the visible grid. However, the areas of coordinates D and E are finite: D is closest to 9 locations, and E is closest to 17 (both including the coordinate's location itself). Therefore, in this example, the size of the largest area is 17.

What is the size of the largest area that isn't infinite?
-}