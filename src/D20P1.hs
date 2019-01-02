module D20P1 (
    dooramount
) where

import Data.List
import Data.Maybe
import Debug.Trace
import Algorithm.Search (aStar)

type Coordinate = (Int, Int)
type Door = [Coordinate]
type Room = (Coordinate, [Coordinate])
type ParseRecord = (Coordinate, [Coordinate], [[Coordinate]], [(Coordinate, [Coordinate], [Coordinate])])

dooramount :: String -> Int
dooramount = maximum . catMaybes . shortestPathes . partition ((==) (0, 0) . fst) . mapRooms
    where
        shortestPathes (start:_, rooms) = map (shortestPath start rooms) rooms

shortestPath :: Room -> [Room] -> Room -> Maybe Int
shortestPath start rooms target@((x, y), _) = (aStar nextRooms cost remaining ((==) target) start) >>= Just . fst
    where
        cost _ _ = 1
        remaining (c, _) = manhattanD c
        manhattanD (x', y') = abs (x - x') + abs (y - y')
        nextRooms (_, l) = map room l
        room c = fromJust $ find ((==) c . fst) (start:rooms)

mapRooms :: String -> [Room]
mapRooms = result . foldl' parseChar ((0, 0), [(0, 0)], [], [])
    where
        result (_, _, doorMap, _) = map (room doorMap) . nub $ concat doorMap
        room doorMap c = (c, delete c . nub . concat $ filter (elem c) doorMap)

parseChar :: ParseRecord -> Char -> ParseRecord
parseChar s@((x, y), doors, doorMap, stack) c = parse
            where
                connect d = (d, [d], (map (sort . flip (:) [d]) doors) ++ doorMap, stack)
                altRoute ((r, doors', ends), stack') = (r, doors', doorMap, (r, doors', (x, y):ends):stack')
                routeEnd (_, _, doorMap, ((d, _, ends):stack')) = (d, ends, doorMap, stack')
                parse = case c of
                    '^' -> s
                    '$' -> s
                    'S' -> connect (x, y + 1)
                    'N' -> connect (x, y - 1)
                    'E' -> connect (x + 1, y)
                    'W' -> connect (x - 1, y)
                    '(' -> ((x, y), doors, doorMap, ((x, y), doors, []):stack)
                    '|' -> altRoute . fromJust $ uncons stack
                    ')' -> routeEnd . altRoute . fromJust $ uncons stack


{-
https://adventofcode.com/2018/day/20

The area you are in is made up entirely of doors and doors. The doors are arranged in a grid, and doors only connect to adjacent doors when a door is present between them.

For example, drawing doors as ., walls as #, doors as | or -, your current position as X, and where north is up, the area you're in might look like this:

#####
#.|.#
#-###
#.|X#
#####
You get the attention of a passing construction Elf and ask for a map. "I don't have time to draw out a map of this place - it's huge. Instead, I can give you directions to every door in the facility!" He writes down some directions on a piece of parchment and runs off. In the example above, the instructions might have been ^WNE$, a regular expression or "regex" (your puzzle input).

The regex matches routes (like WNE for "west, north, east") that will take you from your current door through various doors in the facility. In aggregate, the routes will take you through every door in the facility at least once; mapping out all of these routes will let you build a proper map and find your way around.

^ and $ are at the beginning and end of your regex; these just mean that the regex doesn't match anything outside the routes it describes. (Specifically, ^ matches the start of the route, and $ matches the end of it.) These characters will not appear elsewhere in the regex.

The rest of the regex matches various sequences of the characters N (north), S (south), E (east), and W (west). In the example above, ^WNE$ matches only one route, WNE, which means you can move west, then north, then east from your current position. Sequences of letters like this always match that exact route in the same order.

Sometimes, the route can branch. A branch is given by a list of options separated by pipes (|) and wrapped in parentheses. So, ^N(E|W)N$ contains a branch: after going north, you must choose to go either east or west before finishing your route by going north again. By tracing out the possible routes after branching, you can determine where the doors are and, therefore, where the doors are in the facility.

.... skipped

By following the various routes the regex matches, a full map of all of the doors and doors in the facility can be assembled.

To get a sense for the size of this facility, you'd like to determine which door is furthest from you: specifically, you would like to find the door for which the shortest path to that door would require passing through the most doors.

In the first example (^WNE$), this would be the north-east corner 3 doors away.
In the second example (^ENWWW(NEEE|SSE(EE|N))$), this would be the south-east corner 10 doors away.
In the third example (^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$), this would be the north-east corner 18 doors away.

What is the largest number of doors you would be required to pass through to reach a door? That is, find the door for which the shortest path from your starting location to that door would require passing through the most doors; what is the fewest doors you can pass through to reach it?
-}