module Main where

import D18P1

main :: IO ()
main = do
    f <- readFile "data/d18.txt"
    putStrLn . show $ let
                        acres = parseAcres f
                      in lumbervalue acres