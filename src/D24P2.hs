module D24P2 (
    boostwinningunits
) where

import D24
import Data.List

boostwinningunits :: [Group] -> Int
boostwinningunits groups = cntUnits . snd $ until immuneWins fight (0, groups)
    where
        immuneWins = all ((==) ImmuneSystem . gArmy) . snd
        fight (boost, _) = (boost + 1, snd . until winnerOrDraw boostTurn $ (0, map (boostImmune boost) groups))
        boostImmune boost g = if gArmy g == ImmuneSystem then g { gDamage = (gDamage g) + boost } else g
        winnerOrDraw (prevUnits, l) = (winner l) || (prevUnits == cntUnits l)
        winner = (==) 1 . length . group . sort . map gArmy
        cntUnits = sum . map gUnits
        boostTurn (prevUnits, l) = (cntUnits l, takeTurn l)

{-
https://adventofcode.com/2018/day/20#part2

A boost is an integer increase in immune system units' attack damage.

... skip

You don't even know how you could boost the reindeer's immune system or what effect it might have, so you need to be cautious and find the smallest boost that would allow the immune system to win.

How many units does the immune system have left after getting the smallest boost it needs to win?
-}