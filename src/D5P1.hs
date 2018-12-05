module D5P1 (
  polymersize
) where

import Data.List
import Data.Char

polymersize :: String -> Int
polymersize p = length $ reduceP (length p) p (reduce p)
    where
        reduceP l p r    = if (length r) == l then p else reduceP (length r) r (reduce r)
        reduce []       = []
        reduce [e]      = [e]
        reduce (x:y:xs) = if (diffCase x y) then (reduce xs) else x:(reduce (y:xs))
        diffCase x y    = x == (flipCase y)
        flipCase x      = if isLower x then toUpper x else toLower x


{-
https://adventofcode.com/2018/day/5

The polymer is formed by smaller units which, when triggered, react with each other such that two adjacent units of the same type and opposite polarity are destroyed. Units' types are represented by letters; units' polarity is represented by capitalization. For instance, r and R are units with the same type but opposite polarity, whereas r and s are entirely different types and do not react.

For example:

In aA, a and A react, leaving nothing behind.
In abBA, bB destroys itself, leaving aA. As above, this then destroys itself, leaving nothing.
In abAB, no two adjacent units are of the same type, and so nothing happens.
In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.
Now, consider a larger example, dabAcCaCBAcCcaDA:

dabAcCaCBAcCcaDA  The first 'cC' is removed.
dabAaCBAcCcaDA    This creates 'Aa', which is removed.
dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
dabCBAcaDA        No further actions can be taken.
After all possible reactions, the resulting polymer contains 10 units.

How many units remain after fully reacting the polymer you scanned?
-}