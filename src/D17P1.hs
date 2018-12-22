module D17P1 (
    tilecount
) where

import Data.List
import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.Sequence ((|>), (<|), (><))
import Data.Foldable (toList)
import Control.Applicative
import Debug.Trace

type Coordinate = (Int, Int)

data Tile
    = Water Coordinate
    | Clay Coordinate
    | Sand Coordinate
    | Spring Coordinate
    deriving(Show, Eq)

spring = (0, 500)

tilecount :: [Coordinate] -> Int
tilecount clayPositions = let
        tiles = initialTiles $ V.fromList clayPositions
    in S.length . trace (showTiles tiles) $ tiles

-- nextMove :: Tile -> V.Vector Tile -> Maybe Coordinate
-- nextMove (Water (y, x)) tiles = tryDown <|> tryLeft <|> tryRight
--     where
--         tryDown = freeTile (y + 1, x)
--         tryLeft = freeTile (y, x - 1) *> notEdge (y + 1, x - 1)
--         tryRight = freeTile (y, x + 1) *> notEdge (y + 1, x + 1)
--         freeTile p = maybeTile p Sand
--         maybeTile p t = if V.elem (t p) tiles then Just p else Nothing

initialTiles :: V.Vector Coordinate -> S.Seq Tile
initialTiles clayPositions = foldr addTile S.empty [(y,x) | y <- [0..maxY], x <- [minX..maxX]]
    where
        minX = minimum $ fmap snd clayPositions
        maxX = maximum $ fmap snd clayPositions
        maxY = maximum $ fmap fst clayPositions
        addTile p t
            | p == spring = (Spring p) <| t
            | V.elem p clayPositions = (Clay p) <| t
            | otherwise = (Sand p) <| t

showTiles :: S.Seq Tile -> String
showTiles = toList . snd . foldl' toChar (0, trace "BEG!" $ S.empty)
    where
        addChar lines s c = (s >< (S.replicate lines '\n')) |> c
        toChar (lastY, s) (Sand (y,_)) = (y, addChar (y - lastY) s ' ')
        toChar (lastY, s) (Water (y,_)) = (y, addChar (y - lastY) s '~')
        toChar (lastY, s) (Clay (y,_)) = (y, addChar (y - lastY) s '#')
        toChar (lastY, s) (Spring (y,_)) = (y, addChar (y - lastY) s '+')

{-
https://adventofcode.com/2018/day/17

You scan a two-dimensional vertical slice of the ground nearby and discover that it is mostly sand with veins of clay. The scan only provides data with a granularity of square meters, but it should be good enough to determine how much water is trapped there. In the scan, x represents the distance to the right, and y represents the distance down. There is also a spring of water near the surface at x=500, y=0. The scan identifies which square meters are clay (your puzzle input).

For example, suppose your scan shows the following veins of clay:

x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504
Rendering clay as #, sand as ., and the water spring as +, and with x increasing to the right and y increasing downward, this becomes:

   44444455555555
   99999900000000
   45678901234567
 0 ......+.......
 1 ............#.
 2 .#..#.......#.
 3 .#..#..#......
 4 .#..#..#......
 5 .#.....#......
 6 .#.....#......
 7 .#######......
 8 ..............
 9 ..............
10 ....#.....#...
11 ....#.....#...
12 ....#.....#...
13 ....#######...
The spring of water will produce water forever. Water can move through sand, but is blocked by clay. Water always moves down when possible, and spreads to the left and right otherwise, filling space that has clay on both sides and falling out otherwise.

For example, if five squares of water are created, they will flow downward until they reach the clay and settle there. Water that has come to rest is shown here as ~, while sand through which water has passed (but which is now dry again) is shown as |:
......+.......
......|.....#.
.#..#.|.....#.
.#..#.|#......
.#..#.|#......
.#....|#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...
Two squares of water can't occupy the same location. If another five squares of water are created, they will settle on the first five, filling the clay reservoir a little more:

......+.......
......|.....#.
.#..#.|.....#.
.#..#.|#......
.#..#.|#......
.#~~~~~#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...
Water pressure does not apply in this scenario. If another four squares of water are created, they will stay on the right side of the barrier, and no water will reach the left side:

......+.......
......|.....#.
.#..#.|.....#.
.#..#~~#......
.#..#~~#......
.#~~~~~#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...
At this point, the top reservoir overflows. While water can reach the tiles above the surface of the water, it cannot settle there, and so the next five squares of water settle like this:

......+.......
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
........|.....
....#...|.#...
....#...|.#...
....#~~~~~#...
....#######...
Note especially the leftmost |: the new squares of water can reach this tile, but cannot stop there. Instead, eventually, they all fall to the right and settle in the reservoir below.

After 10 more squares of water, the bottom reservoir is also full:

......+.......
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
........|.....
....#~~~~~#...
....#~~~~~#...
....#~~~~~#...
....#######...
Finally, while there is nowhere left for the water to settle, it can reach a few more tiles before overflowing beyond the bottom of the scanned data:

......+.......    (line not counted: above minimum y value)
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
...|||||||||..
...|#~~~~~#|..
...|#~~~~~#|..
...|#~~~~~#|..
...|#######|..
...|.......|..    (line not counted: below maximum y value)
...|.......|..    (line not counted: below maximum y value)
...|.......|..    (line not counted: below maximum y value)

How many tiles can be reached by the water? To prevent counting forever, ignore tiles with a y coordinate smaller than the smallest y coordinate in your scan data or larger than the largest one. Any x coordinate is valid. In this example, the lowest y coordinate given is 1, and the highest is 13, causing the water spring (in row 0) and the water falling off the bottom of the render (in rows 14 through infinity) to be ignored.

So, in the example above, counting both water at rest (~) and other sand tiles the water can hypothetically reach (|), the total number of tiles the water can reach is 57.

How many tiles can the water reach within the range of y values in your scan?
-}