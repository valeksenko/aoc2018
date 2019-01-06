module D17P2 (
    drainedcount
) where

import D17
import qualified Data.Sequence as S
import Data.Sequence ((><))

drainedcount :: [Coordinate] -> Int
drainedcount = waterCount . drain . snd . fill
    where
        waterCount = S.length . foldr ((><) . S.filter ((==) Water)) S.empty

{-
https://adventofcode.com/2018/day/17#part2

After a very long time, the water spring will run dry. How much water will be retained?

In the example above, water that won't eventually drain out is shown as ~, a total of 29 tiles.

How many water tiles are left after the water spring stops producing water and all remaining water not at rest has drained?
-}