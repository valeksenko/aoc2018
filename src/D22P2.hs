module D22P2 (
    minminutes
) where

import D22
import Data.List
import Data.Maybe
import Data.Tree
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Algorithm.Search (dijkstra)
import Debug.Trace

type CaveMap = S.Seq (S.Seq [Tool])
type Cost = Int

data Tool = Neither | ClimbingGear | Torch deriving(Show, Eq, Ord)

data Move =
    Move {
        mCoordinate :: Coordinate
      , mTool       :: Tool
    } deriving(Show, Eq, Ord)

changeCost = 8
extraRegions = changeCost * 2 -- Q: enough?
caveMouth = (0, 0)

minminutes :: Int -> Coordinate -> Maybe Cost
minminutes depth target = findPath target . mapTools $ mapCave depth target extraRegions
    where
        mapTools = S.mapWithIndex (\x l -> S.mapWithIndex (tools x) l)
        tools x y r
            | (x, y) == caveMouth = [Torch]
            | (x, y) == target = [Torch]
            | otherwise = case r of
                    Rocky -> [ClimbingGear, Torch]
                    Wet -> [ClimbingGear, Neither]
                    Narrow -> [Torch, Neither]

findPath :: Coordinate -> CaveMap -> Maybe Cost
findPath target caveMap = (traceShowId . dijkstra nextMoves cost ((==) target . mCoordinate) $ Move (0, 0) Torch) >>= Just . fst
    where
        cost m1 m2 = if (mTool m1) == (mTool m2) then 1 else 1 + changeCost
        -- assume we have to change the tool on every turn
        -- remaining (Move c _) = (distance c) * (1 + changeCost) + changeCost
        -- distance (x, y) = abs (x - fst target) + abs (y - snd target)
        nextMoves (Move (x, y) _) = concat . catMaybes $ map moves [(x + 1, y), (x -1, y), (x, y + 1), (x, y - 1)]
        moves c@(x', y') = S.lookup y' caveMap >>= S.lookup x' >>= Just . map (Move c)

-- findPath :: (Int, Int) -> (Int, Int) -> Maybe (Int, [(Int, (Int, Int))])
-- findPath start end =
--   let next = taxicabNeighbors
--       cost = taxicabDistance
--       remaining = (taxicabDistance end)
--   in aStar (next `pruning` isWall) cost remaining (== end) start

-- buildMoveTree :: Coordinate -> CaveMap -> Tree Move
-- buildMoveTree target caveMap = targetOnly $ unfoldTree makeMoves (Move caveMouth Torch 0)
--     where
--         targetOnly = filter ((==) target . mCoordinate . head)
--         makeMoves m = if mCoordinate m == target then (m, []) else (m, nextMoves m moves caveMap)


-- allMoves :: Coordinate -> CaveMap -> [[Move]]
-- allMoves target caveMap = targetOnly $ makeMoves [] (Move caveMouth Torch 0)
--     where
--         targetOnly = filter ((==) target . mCoordinate . head)
--         makeMoves moves m = makeM (m:moves) $ nextMoves m moves caveMap
--         makeM past [] = past
--         makeM past next = concatMap (makeMove past) next
--         makeMove moves m = if mCoordinate m == target then [m:moves] else concat makeMoves moves m

-- nextMoves :: Move -> [Move] -> CaveMap -> [Move]
-- nextMoves currentMove pastMoves caveMap = concat . catMaybes $ map tryDirection [Leftward, Rightward, Upward, Downward]
--     where
--         tryDirection = moveTo . nextCoordinate
--         (currentX, currentY) = mCoordinate currentMove
--         moveTo c = if madeMove c then Nothing else nextTools c >>= Just . map (applyTool c)
--         madeMove c = elem c $ map mCoordinate pastMoves
--         nextTools (x, y) = S.lookup y caveMap >>= S.lookup x >>= Just
--         applyTool c t = Move c t $ if t == (mTool currentMove) then 1 else 1 + changeCost
--         nextCoordinate d = case d of
--             Leftward -> (currentX - 1, currentY)
--             Rightward -> (currentX + 1, currentY)
--             Upward -> (currentX, currentY - 1)
--             Downward -> (currentX, currentY + 1)


{-
https://adventofcode.com/2018/day/22#part2

As you leave, he hands you some tools: a torch and some climbing gear. You can't equip both tools at once, but you can choose to use neither.

Tools can only be used in certain regions:

In rocky regions, you can use the climbing gear or the torch. You cannot use neither (you'll likely slip and fall).
In wet regions, you can use the climbing gear or neither tool. You cannot use the torch (if it gets wet, you won't have a light source).
In narrow regions, you can use the torch or neither tool. You cannot use the climbing gear (it's too bulky to fit).
You start at 0,0 (the mouth of the cave) with the torch equipped and must reach the target coordinates as quickly as possible. The regions with negative X or Y are solid rock and cannot be traversed. The fastest route might involve entering regions beyond the X or Y coordinate of the target.

You can move to an adjacent region (up, down, left, or right; never diagonally) if your currently equipped tool allows you to enter that region. Moving to an adjacent region takes one minute. (For example, if you have the torch equipped, you can move between rocky and narrow regions, but cannot enter wet regions.)

You can change your currently equipped tool or put both away if your new equipment would be valid for your current region. Switching to using the climbing gear, torch, or neither always takes seven minutes, regardless of which tools you start with. (For example, if you are in a rocky region, you can switch from the torch to the climbing gear, but you cannot switch to neither.)

Finally, once you reach the target, you need the torch equipped before you can find him in the dark. The target is always in a rocky region, so if you arrive there with climbing gear equipped, you will need to spend seven minutes switching to your torch.

For example, using the same cave system as above, starting in the top left corner (0,0) and moving to the bottom right corner (the target, 10,10) as quickly as possible, one possible route is as follows, with your current position marked X:

Initially:
X=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

... skipped

Switch from using the climbing gear to the torch:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===X===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||
This is tied with other routes as the fastest way to reach the target: 45 minutes. In it, 21 minutes are spent switching tools (three times, seven minutes each) and the remaining 24 minutes are spent moving.

What is the fewest number of minutes you can take to reach the target?
-}