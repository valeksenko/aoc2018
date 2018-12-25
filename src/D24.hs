module D24 (
    takeTurn
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

takeTurn :: [Group] -> [Group]
takeTurn = attackTargets . selectTargets

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
