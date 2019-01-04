module D20 (
    shortestPaths
) where

import Data.List
import Data.Maybe
import Algorithm.Search (aStar)

type Coordinate = (Int, Int)
type Door = [Coordinate]
type Room = (Coordinate, [Coordinate])
type ParseRecord = (Coordinate, [Coordinate], [[Coordinate]], [(Coordinate, [Coordinate], [Coordinate])])

shortestPaths :: String -> [Int]
shortestPaths = catMaybes . paths . partition ((==) (0, 0) . fst) . mapRooms
    where
        paths (start:_, rooms) = map (shortestPath start rooms) rooms

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
