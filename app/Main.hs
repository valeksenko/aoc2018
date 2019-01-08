module Main where

import D15
import D15P1
import qualified Data.Sequence as S

main :: IO ()
main = do
    f <- readFile "data/d15.txt"
    -- not 54353
    putStrLn . show $ let
                        (board, npcs) = parseBoard f
                    in battlescore board npcs

