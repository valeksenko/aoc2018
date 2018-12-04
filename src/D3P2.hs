module D3P2 (
    uniq
) where

import D3
import Data.List
import Control.Applicative

uniq :: [Claim] -> Maybe String
uniq claims = (find uniqClaim claimCoordinates) >>= Just . claimId . fst
    where
        uniqClaim claim   = length (findDups $ (snd claim) ++ dups) == 0
        dups              = map head . findDups $ concatMap snd claimCoordinates
        findDups          = filter ((>1) . length) . group . sort
        claimCoordinates  = map coordinates claims
        coordinates claim = (claim, concatMap (coord claim) $ yCoord claim)
        coord claim y     = map (\x -> y * fabricSize + x) $ xCoord claim
        xCoord claim      = [(leftShift claim)..((leftShift claim) + (width claim) - 1)]
        yCoord claim      = [(topShift claim)..((topShift claim) + (height claim) - 1)]

{-
https://adventofcode.com/2018/day/3#part2

Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single square inch of fabric with any other claim. If you can somehow draw attention to it, maybe the Elves will be able to make Santa's suit after all!

For example, in the claims above, only claim 3 is intact after all claims are made.

What is the ID of the only claim that doesn't overlap?
-}