module D15P1 (
    battlescore
  , parseBoard
  , Npc(..)
  , NpcType(..)
) where

import Data.List
import Data.Maybe
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Ord
import Data.Sequence ((|>), (<|))
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Debug.Trace

type Coordinate = (Int, Int)

data NpcType = Goblin | Elf deriving(Show, Eq, Ord)

data BoardPosition
    = Open Coordinate
    | Occupied Npc Coordinate Int
    deriving(Show, Eq)

data Npc =
    Npc {
        nType :: NpcType
      , nPos  :: Coordinate
      , nHP   :: Int
    } deriving(Show, Eq)

attackPower = 3
initialHP = 200

battlescore :: S.Seq Coordinate -> S.Seq Npc -> Int
battlescore positions npcs  = score $ until winnerOnly (nextRound positions) (0, npcs)
    where
        winnerOnly (_, npcs) = (== 1) . length . group . sort . toList $ fmap nType npcs
        score (cnt, npcs) = (cnt - 1) * (sum . toList $ fmap nHP npcs)

nextRound :: S.Seq Coordinate -> (Int, S.Seq Npc) -> (Int, S.Seq Npc)
nextRound positions (cnt, npcs) = (cnt + 1, getNpcs . foldl' takeTurn npcBoard $ S.sortOn nPos npcs)
    where
        npcBoard = foldr (\p b -> boardP p <| b) S.empty positions
        boardP p = case (S.elemIndexL p $ fmap nPos npcs) of
                        Nothing -> Open p
                        (Just i) -> addOccupied $ S.index npcs i
        addOccupied n = Occupied n (nPos n) (nHP n)
        getNpcs board = S.filter ((>0) . nHP) $ S.fromList [ n { nPos = p, nHP = hp } | (Occupied n p hp) <- toList board ]
        
takeTurn :: S.Seq BoardPosition -> Npc -> S.Seq BoardPosition
takeTurn board npc = maybe board (attack board . move board) $ findNpc (toList board)
    where
        findNpc [] = Nothing
        findNpc ((Occupied n p hp):xs) = if n == npc
                                            then (if hp > 0 then Just (n, p, hp) else Nothing)
                                            else findNpc xs
        findNpc (x:xs) = findNpc xs

move :: S.Seq BoardPosition -> (Npc, Coordinate, Int) -> (Npc, Coordinate, Int)
move board (npc, pos, hp) = moveNpc . closestEnemy $ findEnemies npc board
    where 
        moveNpc Nothing = (npc, pos, hp)
        moveNpc (Just []) = (npc, pos, hp)
        moveNpc (Just l) = (npc, head l, hp)
        closestEnemy = closestE . catMaybes . map (shortestPath board pos . fst)
        closestE [] = Nothing
        closestE l = Just . head $ sortBy cmpListByReading l

shortestPath :: S.Seq BoardPosition -> Coordinate -> Coordinate -> Maybe [Coordinate]
shortestPath board src dst = nearDst $ freeDistances src board
    where
        nearDst available = findPath available . sortBy (comparing fst) $ filter (closePositions dst . snd) available
        findPath _ [] = Nothing
        findPath available closest = Just . init $ foldr (nextStep available) [dst] [1..fst $ head closest]
        nextStep available distance l@(pDst:_) = flip (:) l (head . filter (flip any available . closeDistance (distance - 1)) . sort . map snd $ filter (closeDistance distance pDst) available)
        closeDistance distance pDst (d, p) = (d == distance) && (closePositions p pDst)

freeDistances :: Coordinate -> S.Seq BoardPosition -> [(Int, Coordinate)]
freeDistances src board = getFree (0, ([src], foldr freePos [] board), [])
    where
        getFree (distance, (seeds, available), collected) = if null seeds then collected else getFree (distance + 1, foldr closeFree ([], available) seeds, collected ++ zip (repeat distance) seeds)
        closeFree p (collected, available) = addFree collected $ partition (closePositions p) available
        addFree collected (closeP, farP) = (collected ++ closeP, farP)
        freePos (Open p) l = p:l
        freePos (Occupied _ p hp) l = if hp > 0 then l else p:l

attack :: S.Seq BoardPosition -> (Npc, Coordinate, Int) -> S.Seq BoardPosition
attack board (npc, pos, hp) = hit . groupBy ((==) `on` snd) $ sortBy (comparing snd) closeEnemies
    where
        hit enemies = if (null enemies) then moveNpc board else moveNpc $ hitE (head . sortBy (cmpByReading id) . map fst $ head enemies)
        hitE p = fmap (putEnemy p) board
        putEnemy p' bp@(Occupied n p h) = if (p == p') then Occupied n p (h - attackPower) else bp
        putEnemy _ bp = bp
        moveNpc b = if (nPos npc) == pos then b else fmap putNpc b
        putNpc bp@(Occupied n p _) = if (n == npc) then Open p else bp
        putNpc bp@(Open p) = if (p == pos) then Occupied npc p hp else bp
        closeEnemies = filter (closePositions pos . fst) $ findEnemies npc board

closePositions :: Coordinate -> Coordinate -> Bool
closePositions (y1, x1) (y2, x2) = ((x1 == x2) && (abs (y1 - y2) <= 1)) || ((abs (x1 - x2) <= 1) && (y1 == y2))

-- TODO: stop if no more enemies and don't count this round as active
findEnemies :: Npc -> S.Seq BoardPosition -> [(Coordinate, Int)]
findEnemies npc = toList . fmap stats . S.filter enemy
    where
        enemy (Occupied n _ hp) = (nType npc) /= (nType n) && (hp > 0)
        enemy _ = False
        stats (Occupied _ p hp) = (p, hp)

cmpListByReading :: [Coordinate] -> [Coordinate] -> Ordering
cmpListByReading a b = (comparing length a b) <> (compare (map fst a) (map fst b)) <> (compare (map snd a) (map snd b))

cmpByReading :: (b -> Coordinate) -> b -> b -> Ordering
cmpByReading f = (comparing $ fst . f) <> (comparing $ snd . f)

parseBoard :: String -> (S.Seq Coordinate, S.Seq Npc)
parseBoard = fst . foldl' parsePoint ((S.empty, S.empty), (0, 0))
    where
        parsePoint ((board, npcs), (y, x)) c = case c of
            '.' -> ((board |> (y, x), npcs), (y, x + 1))
            'G' -> ((board |> (y, x), npcs |> Npc Goblin (y, x) initialHP), (y, x + 1))
            'E' -> ((board |> (y, x), npcs |> Npc Elf (y, x) initialHP), (y, x + 1))
            '#' -> ((board, npcs), (y, x + 1))
            ' ' -> ((board, npcs), (y, x))
            '\n' -> ((board, npcs), (y + 1, 0))


{-
https://adventofcode.com/2018/day/15

You scan the area, generating a map of the walls (#), open cavern (.), and starting position of every Goblin (G) and Elf (E) (your puzzle input).

Combat proceeds in rounds; in each round, each unit that is still alive takes a turn, resolving all of its actions before the next unit's turn begins. On each unit's turn, it tries to move into range of an enemy (if it isn't already) and then attack (if it is in range).

All units are very disciplined and always follow very strict combat rules. Units never move or attack diagonally, as doing so would be dishonorable. When multiple choices are equally valid, ties are broken in reading order: top-to-bottom, then left-to-right. For instance, the order in which units take their turns within a round is the reading order of their starting positions in that round, regardless of the type of unit or whether other units have moved after the round started. For example:

                 would take their
These units:   turns in this order:
  #######           #######
  #.G.E.#           #.1.2.#
  #E.G.E#           #3.4.5#
  #.G.E.#           #.6.7.#
  #######           #######
Each unit begins its turn by identifying all possible targets (enemy units). If no targets remain, combat ends.

Then, the unit identifies all of the open squares (.) that are in range of each target; these are the squares which are adjacent (immediately up, down, left, or right) to any target and which aren't already occupied by a wall or another unit. Alternatively, the unit might already be in range of a target. If the unit is not already in range of a target, and there are no open squares which are in range of a target, the unit ends its turn.

If the unit is already in range of a target, it does not move, but continues its turn with an attack. Otherwise, since it is not in range of a target, it moves.

... skipped ...

What is the outcome of the combat described in your puzzle input?
-}