module Main where

import qualified Data.Sequence as S
import D15P1

main :: IO ()
main = do
    f <- readFile "data/d15.txt"
    putStrLn . show $ let
                        (board, npcs) = parseBoard f
                      in battlescore board npcs
