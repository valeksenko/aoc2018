module Main where

import D15
import D15P2
import qualified Data.Sequence as S

main :: IO ()
main = do
    f <- readFile "data/d15.txt"
    putStrLn . show $ let
                        (board, npcs) = parseBoard f
                    in optimizedbattlescore board npcs -- 250594
