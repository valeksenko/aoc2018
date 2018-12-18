module D15P1 (
    battlescore
) where

import D15
import Data.List
import qualified Data.Sequence as S
import Data.Foldable (toList)

battlescore :: S.Seq Coordinate -> S.Seq Npc -> Int
battlescore positions npcs  = score $ until winnerOnly (nextRound (3, 3) positions) (0, npcs)
    where
        winnerOnly (_, npcs) = (== 1) . length . group . sort . toList $ fmap nType npcs
        score (cnt, npcs) = (cnt - 1) * (sum . toList $ fmap nHP npcs)

{-
https://adventofcode.com/2018/day/15

You scan the area, generating a map of the walls (#), open cavern (.), and starting position of every Goblin (G) and Elf (E) (your puzzle input).

Combat proceeds in rounds; in each round, each unit that is still alive takes a turn, resolving all of its actions before the next unit's turn begins. On each unit's turn, it tries to move into range of an enemy (if it isn't already) and then attack (if it is in range).

All units are very disciplined and always follow very strict combat rules. Units never move or attack diagonally, as doing so would be dishonorable. When multiple choices are equally valid, ties are broken in reading order: top-to-bottom, then left-to-right. For instance, the order in which units take their turns within a round is the reading order of their starting positions in that round, regardless of the type of unit or whether other units have moved after the round started. For example:

                 would take their
These units:   turns in this order:
  #######           #######
  #.G.E.#           #.1.2.#
  #E.G.E#           #3.4.5#
  #.G.E.#           #.6.7.#
  #######           #######
Each unit begins its turn by identifying all possible targets (enemy units). If no targets remain, combat ends.

Then, the unit identifies all of the open squares (.) that are in range of each target; these are the squares which are adjacent (immediately up, down, left, or right) to any target and which aren't already occupied by a wall or another unit. Alternatively, the unit might already be in range of a target. If the unit is not already in range of a target, and there are no open squares which are in range of a target, the unit ends its turn.

If the unit is already in range of a target, it does not move, but continues its turn with an attack. Otherwise, since it is not in range of a target, it moves.

... skipped ...

What is the outcome of the combat described in your puzzle input?
-}