module D22 (
    mapCave
  , Coordinate
  , Region(..)
) where

import Data.List
import Data.Sequence ((|>), Seq((:|>)))
import qualified Data.Sequence as S

type Coordinate = (Int, Int)

data Region = Rocky | Wet | Narrow deriving(Show, Eq)

mapCave :: Int -> Coordinate -> Int -> S.Seq (S.Seq Region)
mapCave depth targetP extraRegions = fmap (fmap fst) $ foldl' mapRow S.empty [0..snd targetP + extraRegions]
    where
        mapRow cave y = cave |> foldl' (mapRegion cave y) S.empty [0..fst targetP + extraRegions]
        mapRegion cave y row x = row |> region cave row (x, y)
        erosionLevel geoInd = (geoInd + depth) `mod` 20183
        region _ _ (0, 0) = regionType 0
        region _ _ (0, y) = regionType $ y * 48271
        region _ _ (x, 0) = regionType $ x * 16807
        region (_ :|> prevRow) (_ :|> (_, prevEl)) p@(x, y) = regionType $ if p == targetP then 0 else prevEl * (snd $ S.index prevRow x)
        regionType geoInd = let
                el = erosionLevel geoInd
            in case (el `mod` 3) of
                0 -> (Rocky, el)
                1 -> (Wet, el)
                2 -> (Narrow, el)
