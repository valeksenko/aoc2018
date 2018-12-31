module Main where

import D22
import D22P2

main :: IO ()
main = do
    print $ minminutes 10647 (7, 770)
    print $ minminutes 11820 (7,782)
    print $ minminutes 9171 (7,721)
