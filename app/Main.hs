module Main where

import D11P1

main :: IO ()
main = do
    putStrLn . show $ D11P1.powersquare 9995
    putStrLn "Done"
