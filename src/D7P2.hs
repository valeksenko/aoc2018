module D7P2 (
  totaltime
) where

import Data.List
import Data.Ord
import Data.Char
import Data.Tuple
import Data.Function
import Data.Maybe

type StepId = Char

data Requirement =
    Requirement {
        rId  :: StepId
      , rReq :: [StepId]
    } deriving(Show, Eq)

data Task =
    Task {
        tId  :: StepId
      , tSec :: Int
    } deriving(Show, Eq)

totaltime :: Int -> Int -> [(StepId, StepId)] -> Int
totaltime nWorkers delay steps = tick tasks requirements 0 delay
    where
        requirements = map (requirement . head) . group . sort $ concatMap (\(x,y) -> [x,y]) steps
        requirement r = Requirement r (map fst $ filter ((==) r . snd) steps)
        tasks = replicate nWorkers Nothing

tick :: [Maybe Task] -> [Requirement] -> Int -> Int -> Int
tick tasks reqs second delay = if finished then (second - 1) else nextTick
    where
        finished  = (null reqs) && (all isNothing tasks)
        completed = map tId . filter (((==) 1) . tSec) $ catMaybes tasks
        nextTick  = uncurry tick nextStep (second + 1) delay
        nextStep  = pickNext delay (runTasks tasks) (runStep completed reqs)

pickNext :: Int -> [Maybe Task] -> [Requirement] -> ([Maybe Task], [Requirement])
pickNext delay tasks reqs = addReq delay tasks reqs $ take (freeW tasks) readyReq
    where
        freeW    = length . filter isNothing
        readyReq = sortBy (comparing rId) $ filter (null . rReq) reqs

runStep :: [StepId] -> [Requirement] -> [Requirement]
runStep completed = map step
    where
        step r = r { rReq = (rReq r) \\ completed }

runTasks :: [Maybe Task] -> [Maybe Task]
runTasks = map task
    where
        task Nothing  = Nothing
        task (Just t) = if (tSec t) == 1 then Nothing else Just $ t { tSec = (tSec t) - 1 }

addReq :: Int -> [Maybe Task] -> [Requirement] -> [Requirement] -> ([Maybe Task], [Requirement])
addReq delay tasks oldReqs pickedReqs = (assignW, updateR)
    where
        assignW = take (length tasks) $ (map task pickedReqs) ++ (filter isJust tasks) ++ (repeat Nothing)
        task r = Just $ Task (rId r) (duration $ rId r)
        duration c = (ord c) - (ord 'A') + 1 + delay
        updateR = filter (flip notElem pickedReqs) oldReqs

{-
https://adventofcode.com/2018/day/7#part2

As you're about to begin construction, four of the Elves offer to help. "The sun will set soon; it'll go faster if we work together." Now, you need to account for multiple people working on steps simultaneously. If multiple steps are available, tasks should still begin them in alphabetical order.

Each step takes 60 seconds plus an amount corresponding to its letter: A=1, B=2, C=3, and so on. So, step A takes 60+1=61 seconds, while step Z takes 60+26=86 seconds. No time is required between steps.

To simplify things for the example, however, suppose you only have help from one Elf (a total of two tasks) and that each step takes 60 fewer seconds (so that step A takes 1 second and step Z takes 26 seconds). Then, using the same instructions as above, this is how each second would be spent:

Second   Worker 1   Worker 2   Done
   0        C          .        
   1        C          .        
   2        C          .        
   3        A          F       C
   4        B          F       CA
   5        B          F       CA
   6        D          F       CAB
   7        D          F       CAB
   8        D          F       CAB
   9        D          .       CABF
  10        E          .       CABFD
  11        E          .       CABFD
  12        E          .       CABFD
  13        E          .       CABFD
  14        E          .       CABFD
  15        .          .       CABFDE
Each row represents one second of time. The Second column identifies how many seconds have passed as of the beginning of that second. Each worker column shows the step that worker is currently doing (or . if they are idle). The Done column shows completed steps.

Note that the order of the steps has changed; this is because steps now take time to finish and multiple tasks can begin multiple steps simultaneously.

In this example, it would take 15 seconds for two tasks to complete these steps.

With 5 tasks and the 60+ second step durations described above, how long will it take to complete all of the steps?
-}