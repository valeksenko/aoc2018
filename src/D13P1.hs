module D13P1 (
    firstcrash
  , parseTrack
  , Path(..)
  , Turn(..)
  , Direction(..)
  , IntersectionTurn(..)
  , Car(..)
) where

import Data.List

type Coordinate = (Int, Int)

data Path
    = Vertical Coordinate
    | Horizontal Coordinate
    | Curve Turn Coordinate
    | Intersection Coordinate
    deriving(Show, Eq)

data Turn
    = LeftUp
    | RightUp
    | LeftDown
    | RightDown
    deriving(Show, Eq)

data Direction
    = UpWard
    | DownWard
    | LeftWard
    | RightWard
    deriving(Show, Eq)

data IntersectionTurn
    = LeftTurn
    | GoStraight
    | RightTurn
    deriving(Show, Eq)

data Car =
    Car {
        cPos   :: Coordinate
      , cDir   :: Direction
      , cTurns :: [IntersectionTurn]
    } deriving(Show, Eq)

firstcrash :: [Path] -> [Car] -> Coordinate
firstcrash track cars = last . snd $ until collision (tick track) (cars, [])
    where
        collision = not . null . snd

tick :: [Path] -> ([Car], [Coordinate]) -> ([Car], [Coordinate])
tick track (cars, _) = result $ foldl' position (([], cars), []) track
    where
        result ((cars', _), collisions) = (cars', collisions)
        position ((cars', cars''), collisions) p = withCollisions p cars' collisions . moveCars p $ pCars p cars''
        withCollisions p cars' collisions (on, cars'') = ((cars' ++ on, cars''), foldr (collision $ cars' ++ cars'') collisions on)
        collision cars' car a = if elem (cPos car) (map cPos cars') then (cPos car):a else a
        moveCars p (on, cars') = (map (moveCar p) on, cars')
        pCars p cars' = partition ((==) (pPos p) . cPos) cars'
        pPos p = case p of
            (Vertical c) -> c
            (Horizontal c) -> c
            (Curve _ c) -> c
            (Intersection c) -> c

moveCar :: Path -> Car -> Car
moveCar p car = moveC
    where
        moveC = case p of
            (Vertical _) -> Car (direct $ cDir car) (cDir car) (cTurns car)
            (Horizontal _) -> Car (direct $ cDir car) (cDir car) (cTurns car)
            (Curve t _) -> Car (direct $ turn t) (turn t) (cTurns car)
            (Intersection _) -> Car (direct crossTurn) crossTurn (cycleT $ cTurns car)
        cycleT (x:xs) = xs ++ [x]
        direct d = case d of
            UpWard -> (fst $ cPos car, (snd $ cPos car) - 1)
            DownWard -> (fst $ cPos car, (snd $ cPos car) + 1)
            LeftWard -> ((fst $ cPos car) - 1, snd $ cPos car)
            RightWard -> ((fst $ cPos car) + 1, snd $ cPos car)
        turn t = case t of
            LeftUp -> case (cDir car) of
                UpWard -> RightWard
                LeftWard -> DownWard
            RightUp -> case (cDir car) of
                UpWard -> LeftWard
                RightWard -> DownWard
            LeftDown -> case (cDir car) of
                DownWard -> RightWard
                LeftWard -> UpWard
            RightDown -> case (cDir car) of
                DownWard -> LeftWard
                RightWard -> UpWard
        crossTurn = case (head $ cTurns car) of
            LeftTurn -> case (cDir car) of
                UpWard -> LeftWard
                DownWard -> RightWard
                LeftWard -> DownWard
                RightWard -> UpWard
            RightTurn -> case (cDir car) of
                UpWard -> RightWard
                DownWard -> LeftWard
                LeftWard -> UpWard
                RightWard -> DownWard
            GoStraight -> cDir car

carTurns = [LeftTurn, GoStraight, RightTurn]

parseTrack :: String -> ([Path], [Car])
parseTrack = takeTrack . foldl' parseP (([], []), (0, 0), True)
    where
        takeTrack (t, _, _) = t
        parseP ((path, cars), (x, y), prevBlank) c = case c of
            '-' -> ((path ++ [Horizontal (x, y)], cars), (x + 1, y), False)
            '|' -> ((path ++ [Vertical (x, y)], cars), (x + 1, y), True)
            '+' -> ((path ++ [Intersection (x, y)], cars), (x + 1, y), False)
            '/' -> ((path ++ [Curve (if prevBlank then LeftUp else RightDown) (x, y)], cars), (x + 1, y), False)
            '\\' -> ((path ++ [Curve (if prevBlank then LeftDown else RightUp) (x, y)], cars), (x + 1, y), False)
            '>' -> ((path ++ [Horizontal (x, y)], cars ++ [Car (x, y) RightWard carTurns]), (x + 1, y), False)
            '<' -> ((path ++ [Horizontal (x, y)], cars ++ [Car (x, y) LeftWard carTurns]), (x + 1, y), False)
            '^' -> ((path ++ [Vertical (x, y)], cars ++ [Car (x, y) UpWard carTurns]), (x + 1, y), True)
            'v' -> ((path ++ [Vertical (x, y)], cars ++ [Car (x, y) DownWard carTurns]), (x + 1, y), True)
            ' ' -> ((path, cars), (x + 1, y), True)
            '\n' -> ((path, cars), (0, y + 1), True)

{-
https://adventofcode.com/2018/day/13

Tracks consist of straight paths (| and -), curves (/ and \), and intersections (+). Curves connect exactly two perpendicular pieces of track; for example, this is a closed loop:

/----\
|    |
|    |
\----/
Intersections occur when two perpendicular paths cross. At an intersection, a cart is capable of turning left, turning right, or continuing straight. Here are two loops connected by two intersections:

/-----\
|     |
|  /--+--\
|  |  |  |
\--+--/  |
   |     |
   \-----/
Several carts are also on the tracks. Carts always face either up (^), down (v), left (<), or right (>). (On your initial map, the track under each cart is a straight path matching the direction the cart is facing.)

Each time a cart has the option to turn (by arriving at any intersection), it turns left the first time, goes straight the second time, turns right the third time, and then repeats those directions starting again with left the fourth time, straight the fifth time, and so on. This process is independent of the particular intersection at which the cart has arrived - that is, the cart has no per-intersection memory.

Carts all move at the same speed; they take turns moving a single step at a time. They do this based on their current location: carts on the top row move first (acting from left to right), then carts on the second row move (again from left to right), then carts on the third row, and so on. Once each cart has moved one step, the process repeats; each of these loops is called a tick.

For example, suppose there are two carts on a straight track:

|  |  |  |  |
v  |  |  |  |
|  v  v  |  |
|  |  |  v  X
|  |  ^  ^  |
^  ^  |  |  |
|  |  |  |  |
First, the top cart moves. It is facing down (v), so it moves down one square. Second, the bottom cart moves. It is facing up (^), so it moves up one square. Because all carts have moved, the first tick ends. Then, the process repeats, starting with the first cart. The first cart moves down, then the second cart moves up - right into the first cart, colliding with it! (The location of the crash is marked with an X.) This ends the second and last tick.

After following their respective paths for a while, the carts eventually crash. To help prevent crashes, you'd like to know the location of the first crash. Locations are given in X,Y coordinates, where the furthest left column is X=0 and the furthest top row is Y=0:

           111
 0123456789012
0/---\        
1|   |  /----\
2| /-+--+-\  |
3| | |  X |  |
4\-+-/  \-+--/
5  \------/   
In this example, the location of the first crash is 7,3.
-}