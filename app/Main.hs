module Main where

import D16
import D19P1

main :: IO ()
main = do
    putStrLn . show $ backgroundprog (5, [
                           ADDi (5, 16, 5), SETi (1, 8 , 3), SETi (1, 1 , 1), MULr (3, 1 , 4), EQRr (4, 2 , 4), ADDr (4, 5 , 5), ADDi (5, 1 , 5), ADDr (3, 0 , 0), ADDi (1, 1 , 1), GTRr (1, 2 , 4), ADDr (5, 4 , 5), SETi (2, 7 , 5), ADDi (3, 1 , 3), GTRr (3, 2 , 4), ADDr (4, 5 , 5), SETi (1, 5 , 5), MULr (5, 5 , 5), ADDi (2, 2 , 2), MULr (2, 2 , 2), MULr (5, 2 , 2), MULi (2, 11, 2), ADDi (4, 8 , 4), MULr (4, 5 , 4), ADDi (4, 20, 4), ADDr (2, 4 , 2), ADDr (5, 0 , 5), SETi (0, 4 , 5), SETr (5, 8 , 4), MULr (4, 5 , 4), ADDr (5, 4 , 4), MULr (5, 4 , 4), MULi (4, 14, 4), MULr (4, 5 , 4), ADDr (2, 4 , 2), SETi (0, 7 , 0), SETi (0, 9 , 5)
                        ])