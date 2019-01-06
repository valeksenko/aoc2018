module D17 (
    mapTiles
  , fill
  , drain
  , showTiles
  , spring
  , AreaMap
  , Coordinate
  , Tile(..)
) where

import Data.List
import Data.Tuple
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.Sequence ((|>), (<|), (><))
import Data.Foldable (toList)
import Control.Applicative
import Text.Printf

type Coordinate = (Int, Int)
type Direction = (Int, Int)
type AreaMap = S.Seq (S.Seq Tile)

data Tile = Water | Clay | Sand | Spring deriving(Show, Eq)

spring = (0, 500)
left = (0, -1)
right = (0, 1)
down = (1, 0)

fill :: [Coordinate] -> (Int, AreaMap)
fill clayPositions = let
        (startY, startX, tiles) = mapTiles $ V.fromList clayPositions
        filled = (==) Water . fromJust . at aSpring
        aSpring = (fst spring, snd spring - startX)
    in (startY, fillOverflow $ until filled (pour aSpring) tiles)

drain :: AreaMap -> AreaMap
drain areaMap = S.mapWithIndex drainRow areaMap
    where
        drainRow y = S.mapWithIndex (drainT y)
        drainT y x t = if t == Water then check (y, x) else t
        check c = if (blocked areaMap c left) && (blocked areaMap c right) then Water else Sand

pour :: Coordinate -> AreaMap -> AreaMap
pour start areaMap = putWater . fromJust $ freeTile areaMap down start
    where
        putWater (y, x) = S.adjust' (S.update x Water) y areaMap

fillOverflow :: AreaMap -> AreaMap
fillOverflow areaMap = S.mapWithIndex fillRow areaMap
    where
        fillRow y = S.mapWithIndex (fill y)
        fill y x t = if t == Sand then fromMaybe t $ check (y, x) else t
        check c = if isWater (move down c) then checkSides c else Nothing
        checkSides c = checkSide left c <|> checkSide right c
        checkSide dir c = if isWater (move dir c) && isClay (move dir $ move down c) then Just Water else Nothing
        isWater c = at c areaMap == Just Water
        isClay c = at c areaMap == Just Clay

freeTile :: AreaMap -> Direction -> Coordinate -> Maybe Coordinate
freeTile areaMap dir c = try <|> Just c
    where
        try = case dir of
            (1, 0)  -> tryDown areaMap c <|> trySide areaMap left c <|> trySide areaMap right c
            (0, -1) -> tryDown areaMap c <|> trySide areaMap left c
            (0, 1)  -> tryDown areaMap c <|> trySide areaMap right c

tryDown :: AreaMap -> Coordinate -> Maybe Coordinate
tryDown areaMap c = at (move down c) areaMap >>= checkDown
    where
        checkDown t = if t == Sand then freeTile areaMap down (move down c) else Nothing

trySide :: AreaMap -> Direction -> Coordinate -> Maybe Coordinate
trySide areaMap dir c = at (move dir c) areaMap >>= checkSide
    where
        checkSide t = if t == Sand then at (move down $ move dir c) areaMap >>= checkNextDown else Nothing
        limited d = (blocked areaMap c d) || (overflow areaMap c d)
        checkChannel = if (limited dir) && (limited oppositeDir) then freeTile areaMap dir (move dir c) else Nothing
        oppositeDir = (fst dir, negate $ snd dir)
        checkNextDown t = case t of
            Sand  -> if at (move down c) areaMap == Just Clay then freeTile areaMap down (move down $ move dir c) else Nothing
            Clay  -> if at (move down c) areaMap == Just Water then checkChannel else freeTile areaMap dir (move dir c)
            Water -> if (limited dir) && (limited oppositeDir) then freeTile areaMap dir (move dir c) else Nothing

at :: Coordinate -> AreaMap -> Maybe Tile
at (y, x) areaMap = S.lookup y areaMap >>= S.lookup x 

move :: Direction -> Coordinate -> Coordinate
move (y, x) (y', x') = (y + y', x + x')

blocked :: AreaMap -> Coordinate -> Direction -> Bool
blocked areaMap c dir = maybe False checkDown $ at (move down c) areaMap
    where
        checkDown t = if t == Sand then False else maybe False checkSide $ at (move dir c) areaMap
        checkSide t = if t == Clay then True else blocked areaMap (move dir c) dir

overflow :: AreaMap -> Coordinate -> Direction -> Bool
overflow areaMap c dir = maybe False checkNextDown $ at (move down $ move dir c) areaMap
    where
        checkNextDown t = case t of
            Sand  -> False
            Clay  -> True
            Water -> overflow areaMap (move dir c) dir

mapTiles :: V.Vector Coordinate -> (Int, Int, AreaMap)
mapTiles clayPositions = (minY, minX - 1, foldr addRow S.empty [0..maxY])
    where
        addRow y a = foldr (addTile y) S.empty [minX-1..maxX+1] <| a
        minX = minimum $ fmap snd clayPositions
        maxX = maximum $ fmap snd clayPositions
        minY = minimum $ fmap fst clayPositions
        maxY = maximum $ fmap fst clayPositions
        addTile y x t
            | spring == (y, x) = Spring <| t
            | V.elem (y, x) clayPositions = Clay <| t
            | otherwise = Sand <| t

showTiles :: AreaMap -> String
showTiles = S.foldrWithIndex showRow "\n=================\n"
    where
        showRow y r s = printf "%4d %s" y $ foldr ((:) . toChar) ('\n':s) r
        toChar t = case t of
            Sand   -> ' '
            Water  -> '~'
            Clay   -> '#'
            Spring -> '+'
