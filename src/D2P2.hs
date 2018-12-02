module D2P2 (
  letters
) where

import Data.List
import Data.Tuple
import Control.Monad

letters :: [String] -> Maybe String
letters l = (msum $ pairs l) >>= Just . map fst . filter (uncurry (==)) . uncurry zip
    where
        pairs [] = [Nothing]
        pairs (x:xs) = (pair x xs) : pairs xs
        pair x xs = find ((==) 1 . uniq x) xs >>= curry Just x
        uniq x y = length . filter (uncurry (/=)) $ zip x y

{-
https://adventofcode.com/2018/day/2#part2

Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.

The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:

abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.

What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)
-}