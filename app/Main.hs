module Main where

import Data.SBV

guessNumber :: IO AllSatResult
guessNumber = allSat $ do
    x <- sInteger "X"
    y <- sInteger "Y"

    let num = x * 10 + y

    solve [
            num .>= 1
          , num .<= 50
          , (num `sRem` 3) .== 0
          , (num `sMod` 2) .== 1
          , (x + y) .>= 4
          , (x + y) .<= 8
          , (x * y) .>= 4
          , (x * y) .<= 8
        ]

main :: IO ()
main = do
    print =<< guessNumber
