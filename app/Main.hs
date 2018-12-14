module Main where

import D13
import D13P2
import Data.Tuple

main :: IO ()
main = do
    f <- readFile "data/d13.txt"
    putStrLn . show . uncurry lastcar $ parseTrack f
    putStrLn "Done"
