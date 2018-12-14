module D13 (
    parseTrack
  , moveCar
  , Path(..)
  , Turn(..)
  , Direction(..)
  , IntersectionTurn(..)
  , Coordinate(..)
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
