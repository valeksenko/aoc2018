module D16P2 (
    regzero
) where

import D16
import Data.List
import Data.Maybe
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Function
import Data.Ord

regzero :: [([Register], (Int, Address), [Register])] -> [(Int, Address)] -> Register
regzero samples prog = execProg . mapOps $ map possibleOps samples
    where
        possibleOps (iReg, (i, addr), tReg) = (i, map fst . filter ((== tReg) . snd) $ runSample addr iReg)
        mapOps = map (\(i, l) -> (i, head l)) . until allResolved resolve . map (\l -> (fst $ head l, foldr1 intersect $ map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)
        allResolved = all ((== 1) . length . snd)
        resolve l = let r = resolved l in map (\(i, e) -> if length e == 1 then (i, e) else (i, e \\ r )) l
        resolved = concat . filter ((== 1) . length) . map snd
        execProg ops = head . runProgram $ map (codeToOp ops) prog
        codeToOp ops (i, addr) = (allInstructions !! (fromJust $ lookup i ops)) addr

{-
https://adventofcode.com/2018/day/16#part2

Using the samples you collected, work out the number of each opcode and execute the test program (the second section of your puzzle input).

What value is contained in register 0 after executing the test program?
-}