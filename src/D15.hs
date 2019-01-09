module D15 (
    nextRound
  , parseBoard
  , Coordinate
  , Npc(..)
  , NpcType(..)
) where

import Data.List
import Data.Maybe
import Data.Either
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Ord
import Data.Sequence ((|>), (<|))
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Algorithm.Search (dijkstra)
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
nextRound attackPowers positions (cnt, npcs) = roundResult . foldl' turn (Left npcBoard) $ S.sortOn nPos npcs
    where
        npcBoard = foldr (\p b -> boardP p <| b) S.empty positions
        boardP p = case (S.elemIndexL p $ fmap nPos npcs) of
                        Nothing -> Open p
                        (Just i) -> addOccupied $ S.index npcs i
        addOccupied n = Occupied n (nPos n) (nHP n)
        getNpcs board = S.fromList [ n { nPos = p, nHP = hp } | (Occupied n p hp) <- toList board, hp > 0 ]
        turn (Left board) npc = takeTurn attackPowers board npc
        turn (Right board) _ = Right board
        roundResult (Left board) = (cnt + 1, getNpcs board)
        roundResult (Right board) = (cnt, getNpcs board)
        
takeTurn :: (Int, Int) -> S.Seq BoardPosition -> Npc -> Either (S.Seq BoardPosition) (S.Seq BoardPosition)
takeTurn attackPowers board npc = maybe (Left board) (attack attackPower board . move board) $ findNpc (toList board)
    where
        findNpc [] = Nothing
        findNpc ((Occupied n p hp):xs) = if n == npc
                                            then (if hp > 0 then Just (n, p, hp) else Nothing)
                                            else findNpc xs
        findNpc (_:xs) = findNpc xs
        attackPower = if (nType npc == Elf) then fst attackPowers else snd attackPowers

move :: S.Seq BoardPosition -> (Npc, Coordinate, Int) -> (Npc, Coordinate, Int)
move board (npc, pos, hp) = moveNpc $ nextPos $ findEnemies npc board
    where 
        moveNpc Nothing = (npc, pos, hp)
        moveNpc (Just p) = (npc, p, hp)
        nextPos l = if any (closePositions pos . fst) l then Nothing else closestE $ concatMap (allPaths board pos . fst) l
        closestE l = if null l then Nothing else Just . startPos . head $ sortBy cmpListByReading l
        startPos l = head . filter (bestPath (last l) (length l)) . sort $ freeNeighbors board pos
        bestPath dP maxL sP = any ((>) maxL . length) $ allPaths board sP dP

allPaths :: S.Seq BoardPosition -> Coordinate -> Coordinate -> [[Coordinate]]
allPaths board src = map snd . catMaybes . map shortest . freeNeighbors board
    where
        shortest p = dijkstra (freeNeighbors board) cost ((==) p) src
        cost _ _ = 1

freeNeighbors :: S.Seq BoardPosition -> Coordinate -> [Coordinate]
freeNeighbors board dst = filter (closePositions dst) . catMaybes . toList $ fmap freeN board
    where
        freeN (Open p) = Just p
        freeN (Occupied _ p hp) = if hp > 0 then Nothing else Just p

attack :: Int -> S.Seq BoardPosition -> (Npc, Coordinate, Int) -> Either (S.Seq BoardPosition) (S.Seq BoardPosition)
attack attackPower board (npc, pos, hp) = attackE $  findEnemies npc board
    where
        attackE [] = Right board
        attackE l = Left . hit . groupBy ((==) `on` snd) . sortBy (comparing snd) $ filter (closePositions pos . fst) l
        hit enemies = if (null enemies) then moveNpc board else moveNpc $ hitE (head . sort . map fst $ head enemies)
        hitE p = fmap (putEnemy p) board
        putEnemy p' bp@(Occupied n p h) = if (p == p') then Occupied n p (h - attackPower) else bp
        putEnemy _ bp = bp
        moveNpc b = if (nPos npc) == pos then b else fmap putNpc b
        putNpc bp@(Occupied n p _) = if (n == npc) then Open p else (if (p == pos) then Occupied npc p hp else bp)
        putNpc bp@(Open p) = if (p == pos) then Occupied npc p hp else bp

closePositions :: Coordinate -> Coordinate -> Bool
closePositions (y1, x1) (y2, x2) = ((x1 == x2) && (abs (y1 - y2) <= 1)) || ((abs (x1 - x2) <= 1) && (y1 == y2))

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
