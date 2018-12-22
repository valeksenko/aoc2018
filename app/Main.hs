module Main where

import D22P1

main :: IO ()
main = do
    putStrLn . show $ risklevel 10647 (7,770)
