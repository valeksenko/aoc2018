module D22P2 (
    minminutes
) where

import D22
import Data.List
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Debug.Trace

data Tool = Neither | ClimbingGear | Torch deriving(Show, Eq)

extraRegions = 8 -- tool change + 1 should be enough?
caveMouth = (0, 0)

minminutes :: Int -> Coordinate -> Int
minminutes depth targetP = S.length . traceShowId . mapTools $ mapCave depth targetP extraRegions
    where
        mapTools = S.mapWithIndex (\x l -> S.mapWithIndex (tools x) l)
        tools x y r
            | (x, y) == caveMouth = [Torch]
            | (x, y) == targetP = [Torch]
            | otherwise = useTool r

useTool :: Region -> [Tool]
useTool r = case r of
                    Rocky -> [ClimbingGear, Torch]
                    Wet -> [ClimbingGear, Neither]
                    Narrow -> [Torch, Neither]

canUseTool :: Region -> [Tool] -> Bool
canUseTool region = not . null . union (useTool region)



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