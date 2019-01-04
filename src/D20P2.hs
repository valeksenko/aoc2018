module D20P2 (
    roomamount
) where

import D20
import Data.List

roomamount :: String -> Int
roomamount = length . filter ((<=) 1000) . shortestPaths

{-
https://adventofcode.com/2018/day/20#part2

How many rooms have a shortest path from your current location that pass through at least 1000 doors?
-}