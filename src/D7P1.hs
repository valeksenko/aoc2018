module D7P1 (
  steporder
) where

import Data.List
import Data.Ord
import Data.Function

type StepId = Char

data Requirement =
    Requirement {
        rId  :: StepId
      , rReq :: [StepId]
    } deriving(Show, Eq)

steporder :: [(StepId, StepId)] -> [StepId]
steporder steps = reverse $ (walk []) requirements
    where
        requirements = map (requirement . head) . group . sort $ concatMap (\(x,y) -> [x,y]) steps
        requirement r = Requirement r (map fst $ filter ((==) r . snd) steps)

walk :: [StepId] -> [Requirement] -> [StepId]
walk steps [] = steps
walk steps reqs = nextStep $ next reqs
    where
        nextStep s = walk ((rId s):steps) $ walkR s
        next = head . sortBy (comparing rId) . filter (null . rReq)
        walkR s = foldr (step s) [] reqs
        step s r a = if (s == r) then a else (Requirement (rId r) (delete (rId s) $ rReq r)):a

{-
https://adventofcode.com/2018/day/7

The instructions specify a series of steps and requirements about which steps must be finished before others can begin (your puzzle input). Each step is designated by a single letter. For example, suppose you have the following instructions:

Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
Visually, these requirements look like this:


  -->A--->B--
 /    \      \
C      -->D----->E
 \           /
  ---->F-----
Your first goal is to determine the order in which the steps should be completed. If more than one step is ready, choose the step which is first alphabetically. In this example, the steps would be completed as follows:

Only C is available, and so it is done first.
Next, both A and F are available. A is first alphabetically, so it is done next.
Then, even though F was available earlier, steps B and D are now also available, and B is the first alphabetically of the three.
After that, only D and F are available. E is not available because only some of its prerequisites are complete. Therefore, D is completed next.
F is the only choice, so it is done next.
Finally, E is completed.
So, in this example, the correct order is CABDFE.

In what order should the steps in your instructions be completed?
-}