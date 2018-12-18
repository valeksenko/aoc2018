module D15 (
    nextRound
  , parseBoard
  , Coordinate
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

initialHP = 200

nextRound :: (Int, Int) -> S.Seq Coordinate -> (Int, S.Seq Npc) -> (Int, S.Seq Npc)
nextRound attackPowers positions (cnt, npcs) = (cnt + 1, getNpcs . foldl' (takeTurn attackPowers) npcBoard $ S.sortOn nPos npcs)
    where
        npcBoard = foldr (\p b -> boardP p <| b) S.empty positions
        boardP p = case (S.elemIndexL p $ fmap nPos npcs) of
                        Nothing -> Open p
                        (Just i) -> addOccupied $ S.index npcs i
        addOccupied n = Occupied n (nPos n) (nHP n)
        getNpcs board = S.fromList [ n { nPos = p, nHP = hp } | (Occupied n p hp) <- toList board, hp > 0 ]
        
takeTurn :: (Int, Int) -> S.Seq BoardPosition -> Npc -> S.Seq BoardPosition
takeTurn attackPowers board npc = maybe board (attack attackPower board . move board) $ findNpc (toList board)
    where
        findNpc [] = Nothing
        findNpc ((Occupied n p hp):xs) = if n == npc
                                            then (if hp > 0 then Just (n, p, hp) else Nothing)
                                            else findNpc xs
        findNpc (x:xs) = findNpc xs
        attackPower = if (nType npc == Elf) then fst attackPowers else snd attackPowers

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

attack :: Int -> S.Seq BoardPosition -> (Npc, Coordinate, Int) -> S.Seq BoardPosition
attack attackPower board (npc, pos, hp) = hit . groupBy ((==) `on` snd) $ sortBy (comparing snd) closeEnemies
    where
        hit enemies = if (null enemies) then moveNpc board else moveNpc $ hitE (head . sort . map fst $ head enemies)
        hitE p = fmap (putEnemy p) board
        putEnemy p' bp@(Occupied n p h) = if (p == p') then Occupied n p (h - attackPower) else bp
        putEnemy _ bp = bp
        moveNpc b = if (nPos npc) == pos then b else fmap putNpc b
        putNpc bp@(Occupied n p _) = if (n == npc) then Open p else (if (p == pos) then Occupied npc p hp else bp)
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
cmpListByReading a b = (comparing length a b) <> (compare (reverse a) (reverse b))

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
