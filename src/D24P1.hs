module D24P1 (
    winningunits
  , Army(..)
  , Attack(..)
  , Group(..)
) where

import Data.List
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord
import qualified Data.Vector as V
import Data.Vector ((//))
import Debug.Trace

data Attack
    = Radiation
    | Bludgeoning
    | Cold
    | Slashing
    | Fire
    deriving(Show, Eq)

data Army = ImmuneSystem | Infection deriving(Show, Eq, Ord)

data Group =
    Group {
        gId         :: Int
      , gArmy       :: Army
      , gUnits      :: Int
      , gHP         :: Int
      , gImmune     :: [Attack]
      , gWeakness   :: [Attack]
      , gDamage     :: Int
      , gAttack     :: Attack
      , gInitiative :: Int
    } deriving(Show, Eq)

winningunits :: [Group] -> Int
winningunits = sum . map gUnits . until winner turn
    where
        winner = (==) 1 . length . group . sort . map gArmy
        turn = attackTargets . selectTargets

effectivePower :: Group -> Int
effectivePower g = gUnits g * gDamage g

selectTargets :: [Group] -> [(Group, Maybe Group)]
selectTargets groups = foldl' selectT [] $ inOrder groups
    where
        inOrder = sortBy ((flip $ comparing effectivePower) <> (flip $ comparing gInitiative))
        selectT targets g = (g, target g $ filter (enemy g targets) groups):targets
        target g enemies = if null enemies then Nothing else Just $ maxDamage g enemies
        enemy g1 targets g2 = (gArmy g1 /= gArmy g2) && (notElem g2 . catMaybes $ map snd targets) && (damage g1 g2 > 0)
        maxDamage g enemies = maximumBy ((comparing $ damage g) <> (comparing effectivePower) <> (comparing gInitiative)) enemies

damage :: Group -> Group -> Int
damage g1 g2
        | elem (gAttack g1) (gImmune g2)   = 0
        | elem (gAttack g1) (gWeakness g2) = 2 * effectivePower g1
        | otherwise                        = effectivePower g1

attackTargets :: [(Group, Maybe Group)] -> [Group]
attackTargets targets = filter ((>0) . gUnits) . V.toList $ foldr attackT groups inOrder
    where
        groups = V.fromList $ map fst targets
        inOrder = sortOn (gInitiative . fst) targets
        attackT (g1, g2) results = maybe results (attack (fromJust $ find ((==) (gId g1) . gId) results) results) g2
        attack g1 results g2 = if (gUnits g1 > 0) then updateR g2 (liveUnits g1 g2) results else results
        liveUnits g1 g2 = dealDamage g2 $ damage g1 g2
        dealDamage g d = if (d > 0) then (gUnits g) - (d `div` (gHP g)) else 0
        updateR g units l = l // [(fromJust $ V.findIndex ((==) (gId g) . gId) l, g { gUnits = units })]

{-
https://adventofcode.com/2018/day/20

The immune system and the infection each have an army made up of several groups; each group consists of one or more identical units. The armies repeatedly fight until only one army has units remaining.

Units within a group all have the same hit points (amount of damage a unit can take before it is destroyed), attack damage (the amount of damage each unit deals), an attack type, an initiative (higher initiative units attack first and win ties), and sometimes weaknesses or immunities. Here is an example group:

18 units each with 729 hit points (weak to fire; immune to cold, slashing)
 with an attack that does 8 radiation damage at initiative 10
Each group also has an effective power: the number of units in that group multiplied by their attack damage. The above group has an effective power of 18 * 8 = 144. Groups never have zero or negative units; instead, the group is removed from combat.

Each fight consists of two phases: target selection and attacking.

During the target selection phase, each group attempts to choose one target. In decreasing order of effective power, groups choose their targets; in a tie, the group with the higher initiative chooses first. The attacking group chooses to target the group in the enemy army to which it would deal the most damage (after accounting for weaknesses and immunities, but not accounting for whether the defending group has enough units to actually receive all of that damage).

If an attacking group is considering two defending groups to which it would deal equal damage, it chooses to target the defending group with the largest effective power; if there is still a tie, it chooses the defending group with the highest initiative. If it cannot deal any defending groups damage, it does not choose a target. Defending groups can only be chosen as a target by one attacking group.

At the end of the target selection phase, each group has selected zero or one groups to attack, and each group is being attacked by zero or one groups.

During the attacking phase, each group deals damage to the target it selected, if any. Groups attack in decreasing order of initiative, regardless of whether they are part of the infection or the immune system. (If a group contains no units, it cannot attack.)

The damage an attacking group deals to a defending group depends on the attacking group's attack type and the defending group's immunities and weaknesses. By default, an attacking group would deal damage equal to its effective power to the defending group. However, if the defending group is immune to the attacking group's attack type, the defending group instead takes no damage; if the defending group is weak to the attacking group's attack type, the defending group instead takes double damage.

The defending group only loses whole units from damage; damage is always dealt in such a way that it kills the most units possible, and any remaining damage to a unit that does not immediately kill it is ignored. For example, if a defending group contains 10 units with 10 hit points each and receives 75 damage, it loses exactly 7 units and is left with 3 units at full health.

After the fight is over, if both armies still contain units, a new fight begins; combat only ends once one army has lost all of its units.

You scan the reindeer's condition (your puzzle input); the white-bearded man looks nervous. As it stands now, how many units would the winning army have?
-}