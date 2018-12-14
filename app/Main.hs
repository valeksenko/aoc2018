module Main where

import D13P1
import Data.Tuple

main :: IO ()
main = do
    f <- readFile "data/d13.txt"
    putStrLn . show . uncurry firstcrash $ parseTrack f
    putStrLn "Done"
