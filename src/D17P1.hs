module D17P1 (
    tilecount
) where

import Data.List
import Data.Tuple
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.Sequence ((|>), (<|), (><))
import Data.Foldable (toList)
import Control.Applicative
import Text.Printf
import Debug.Trace

type Coordinate = (Int, Int)
type Direction = (Int, Int)
type AreaMap = S.Seq (S.Seq Tile)

data Tile = Water | Clay | Sand | Spring deriving(Show, Eq)

spring = (0, 500)
left = (0, -1)
right = (0, 1)
down = (1, 0)


tilecount :: [Coordinate] -> Int
tilecount clayPositions = let
        (startY, startX, tiles) = mapTiles $ V.fromList clayPositions
        filled = (==) Water . fromJust . at aSpring
        aSpring = (fst spring, snd spring - startX)
        waterCount l = trace (showTiles l) . S.length . foldr ((><) . S.filter ((==) Water)) S.empty $ S.drop startY l
    in waterCount . fillOverflow $ until filled (pour aSpring) tiles

pour :: Coordinate -> AreaMap -> AreaMap
pour start areaMap = putWater . fromJust $ freeTile areaMap down start
    where
        putWater (y, x) = S.adjust' (S.update x Water) y areaMap

fillOverflow :: AreaMap -> AreaMap
fillOverflow areaMap = S.mapWithIndex fillRow areaMap
    where
        fillRow y = S.mapWithIndex (fill y)
        fill y x t = if t == Sand then fromMaybe t $ check (y, x) else t
        check c = if isWater (move down c) then checkSides c else Nothing
        checkSides c = checkSide left c <|> checkSide right c
        checkSide dir c = if isWater (move dir c) && isClay (move dir $ move down c) then Just Water else Nothing
        isWater c = at c areaMap == Just Water
        isClay c = at c areaMap == Just Clay

freeTile :: AreaMap -> Direction -> Coordinate -> Maybe Coordinate
freeTile areaMap dir c = try <|> Just c
    where
        try = case dir of
            (1, 0)  -> tryDown areaMap c <|> trySide areaMap left c <|> trySide areaMap right c
            (0, -1) -> tryDown areaMap c <|> trySide areaMap left c
            (0, 1)  -> tryDown areaMap c <|> trySide areaMap right c

tryDown :: AreaMap -> Coordinate -> Maybe Coordinate
tryDown areaMap c = at (move down c) areaMap >>= checkDown
    where
        checkDown t = if t == Sand then freeTile areaMap down (move down c) else Nothing

trySide :: AreaMap -> Direction -> Coordinate -> Maybe Coordinate
trySide areaMap dir c = at (move dir c) areaMap >>= checkSide
    where
        checkSide t = if t == Sand then at (move down $ move dir c) areaMap >>= checkNextDown else Nothing
        limited d = (blocked areaMap c d) || (overflow areaMap c d)
        checkChannel = if (limited dir) && (limited oppositeDir) then freeTile areaMap dir (move dir c) else Nothing
        oppositeDir = (fst dir, negate $ snd dir)
        checkNextDown t = case t of
            Sand  -> if at (move down c) areaMap == Just Clay then freeTile areaMap down (move down $ move dir c) else Nothing
            Clay  -> if at (move down c) areaMap == Just Water then checkChannel else freeTile areaMap dir (move dir c)
            Water -> if (limited dir) && (limited oppositeDir) then freeTile areaMap dir (move dir c) else Nothing

at :: Coordinate -> AreaMap -> Maybe Tile
at (y, x) areaMap = S.lookup y areaMap >>= S.lookup x 

move :: Direction -> Coordinate -> Coordinate
move (y, x) (y', x') = (y + y', x + x')

blocked :: AreaMap -> Coordinate -> Direction -> Bool
blocked areaMap c dir = maybe False checkDown $ at (move down c) areaMap
    where
        checkDown t = if t == Sand then False else maybe False checkSide $ at (move dir c) areaMap
        checkSide t = if t == Clay then True else blocked areaMap (move dir c) dir

overflow :: AreaMap -> Coordinate -> Direction -> Bool
overflow areaMap c dir = maybe False checkNextDown $ at (move down $ move dir c) areaMap
    where
        checkNextDown t = case t of
            Sand  -> False
            Clay  -> True
            Water -> overflow areaMap (move dir c) dir

mapTiles :: V.Vector Coordinate -> (Int, Int, AreaMap)
mapTiles clayPositions = (minY, minX - 1, foldr addRow S.empty [0..maxY])
    where
        addRow y a = foldr (addTile y) S.empty [minX-1..maxX+1] <| a
        minX = minimum $ fmap snd clayPositions
        maxX = maximum $ fmap snd clayPositions
        minY = minimum $ fmap fst clayPositions
        maxY = maximum $ fmap fst clayPositions
        addTile y x t
            | spring == (y, x) = Spring <| t
            | V.elem (y, x) clayPositions = Clay <| t
            | otherwise = Sand <| t

showTiles :: AreaMap -> String
showTiles = S.foldrWithIndex showRow "\n=================\n"
    where
        showRow y r s = printf "%4d %s" y $ foldr ((:) . toChar) ('\n':s) r
        toChar t = case t of
            Sand   -> ' '
            Water  -> '~'
            Clay   -> '#'
            Spring -> '+'

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