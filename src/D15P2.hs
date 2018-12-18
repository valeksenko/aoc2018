module D15P2 (
    optimizedbattlescore
) where

import D15
import Data.List
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Debug.Trace

optimizedbattlescore :: S.Seq Coordinate -> S.Seq Npc -> Int
optimizedbattlescore positions initNpcs  = fst . snd $ until elvesLive battle (3, (0, 0))
    where
        elvesLive (_, (_, liveE)) = liveE == countElves initNpcs
        battle (attackPower, _) = result attackPower $ until winnerOnly (nextRound (attackPower, 3) positions) (0, initNpcs)
        winnerOnly (_, npcs) = (== 1) . length . group . sort . toList $ fmap nType npcs
        result attackPower (cnt, npcs) = traceShow npcs $ (attackPower + 1, ((cnt - 1) * (sum . toList $ fmap nHP npcs), countElves npcs))
        countElves = S.length . S.filter (\n -> nType n == Elf)

{-
https://adventofcode.com/2018/day/15#part2

According to your calculations, the Elves are going to lose badly. Surely, you won't mess up the timeline too much if you give them just a little advanced technology, right?

You need to make sure the Elves not only win, but also suffer no losses: even the death of a single Elf is unacceptable.

However, you can't go too far: larger changes will be more likely to permanently alter spacetime.

So, you need to find the outcome of the battle in which the Elves have the lowest integer attack power (at least 4) that allows them to win without a single death. The Goblins always have an attack power of 3.

In the first summarized example above, the lowest attack power the Elves need to win without losses is 15:

#######       #######
#.G...#       #..E..#   E(158)
#...EG#       #...E.#   E(14)
#.#.#G#  -->  #.#.#.#
#..G#E#       #...#.#
#.....#       #.....#
#######       #######

Combat ends after 29 full rounds
Elves win with 172 total hit points left
Outcome: 29 * 172 = 4988

After increasing the Elves' attack power until it is just barely enough for them to win without any Elves dying, what is the outcome of the combat described in your puzzle input?
-}