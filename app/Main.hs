module Main where

import D22P2

main :: IO ()
main = do
    putStrLn . show $ minminutes 10647 (7,770)
