module D13P2 (
    lastcar
) where

import D13
import Data.List

lastcar :: [Path] -> [Car] -> Coordinate
lastcar track cars = cPos . head $ until lastCar (tick track) cars
    where
        lastCar cars' = length cars' == 1

tick :: [Path] -> [Car] -> [Car]
tick track cars = fst $ foldl' position ([], cars) track
    where
        position (cars', cars'') p = removeCollisions cars' . moveCars p $ pCars p cars''
        removeCollisions cars' ([], cars'') = (cars', cars'')
        removeCollisions cars' (on, cars'') = ((removeC on cars') ++ (removeC (cars' ++ cars'') on), removeC on cars'')
        removeC x y = filter (\e -> notElem (cPos e) $ map cPos x) y
        moveCars p (on, cars') = (map (moveCar p) on, cars')
        pCars p cars' = partition ((==) (pPos p) . cPos) cars'
        pPos p = case p of
            (Vertical c) -> c
            (Horizontal c) -> c
            (Curve _ c) -> c
            (Intersection c) -> c

{-
https://adventofcode.com/2018/day/13#part2

There isn't much you can do to prevent crashes in this ridiculous system. However, by predicting the crashes, the Elves know where to be in advance and instantly remove the two crashing carts the moment any crash occurs.

They can proceed like this for a while, but eventually, they're going to run out of carts. It could be useful to figure out where the last cart that hasn't crashed will end up.

For example:

/>-<\  
|   |  
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/

/---\  
|   |  
| v-+-\
| | | |
\-+-/ |
  |   |
  ^---^

/---\  
|   |  
| /-+-\
| v | |
\-+-/ |
  ^   ^
  \---/

/---\  
|   |  
| /-+-\
| | | |
\-+-/ ^
  |   |
  \---/
After four very expensive crashes, a tick ends with only one cart remaining; its final location is 6,4.

What is the location of the last cart at the end of the first tick where it is the only cart left?
-}