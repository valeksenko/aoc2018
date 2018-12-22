module Main where

import D16
import D19P1

main :: IO ()
main = do
    -- running: stack run > /tmp/p2 2>&1
    -- and checking until it starts looping via: cut -d, -f 2 /tmp/p2|sort -n|uniq -u|wc -l
    -- grabbing all: cut -d, -f 2 /tmp/p2
    -- finding last dup
    putStrLn . show $ backgroundprog 4 [
                            SETi (123, 0, 1), BANi (1, 456, 1), EQRi (1, 72, 1), ADDr (1, 4, 4), SETi (0, 0, 4), SETi (0, 7, 1), BORi (1, 65536, 2), SETi (8725355, 6, 1), BANi (2, 255, 5), ADDr (1, 5, 1), BANi (1, 16777215, 1), MULi (1, 65899, 1), BANi (1, 16777215, 1), GTIr (256, 2, 5), ADDr (5, 4, 4), ADDi (4, 1, 4), SETi (27, 8, 4), SETi (0, 0, 5), ADDi (5, 1, 3), MULi (3, 256, 3), GTRr (3, 2, 3), ADDr (3, 4, 4), ADDi (4, 1, 4), SETi (25, 1, 4), ADDi (5, 1, 5), SETi (17, 9, 4), SETr (5, 1, 2), SETi (7, 6, 4), EQRr (1, 0, 5), ADDr (5, 4, 4), SETi (5, 7, 4)
                        ] [0,0,0,0,0,0]
