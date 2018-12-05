module D4 (
    idtime1
  , idtime2
  , GuardLog(..)
  , LogTime(..)
) where

import Data.List
import Data.Ord
import Control.Arrow

data GuardLog
    = Starts GuardId LogTime
    | Sleeps LogTime
    | Wakes LogTime
    deriving(Show, Eq)

type GuardId = Int

data LogTime =
    LogTime {
        lDate   :: String
      , lHour   :: Int
      , lMinute :: Int
    } deriving(Show, Eq)

data CompactLog =
    CompactLog {
        guardId       :: GuardId
      , startMinute   :: Int
      , sleepDuration :: Int
    } deriving(Show, Eq)

data GuardMinute =
    GuardMinute {
        minuteGuardId :: GuardId
      , minuteMax     :: Int
      , minuteAmount  :: Int
    } deriving(Show, Eq)


idtime1 :: [GuardLog] -> Maybe Int
idtime1 log = findGuard >>= Just . guardMinute
    where
        findGuard       = Just . fst . maximumBy (comparing snd) $ map guardDuration compLog
        guardDuration e = (guardId $ head e, duration e)
        duration        = foldr (\x d -> d + (sleepDuration x)) 0
        compLog         = guardLog log
        guardMinute g   = g * (minute g)
        minute          = head . maxMinute . guardRecords
        guardRecords g  = concat $ filter ((==) g . guardId . head) compLog

idtime2 :: [GuardLog] -> Maybe Int
idtime2 log = findGuard >>= Just . guardMinute
    where
        findGuard     = Just . maximumBy (comparing minuteAmount) . map minute $ guardLog log
        guardMinute g = (minuteGuardId g) * (minuteMax g)
        minute l      = uncurry (GuardMinute (guardId $ head l)) $ (head &&& length) (maxMinute l)

sortedLog :: [GuardLog] -> [GuardLog]
sortedLog = sortBy cmpTime
    where
        cmpTime x y        = cmp (lTime x) (lTime y)
        lTime (Starts _ t) = t
        lTime (Sleeps t)   = t
        lTime (Wakes t)    = t
        cmp x y
            | ((lDate x) == (lDate y)) && ((lHour x) == (lHour y)) = compare (lMinute x) (lMinute y)
            | ((lDate x) == (lDate y)) = compare (lHour x) (lHour y)
            | otherwise = compare (lDate x) (lDate y)


compactLog :: [GuardLog] -> [CompactLog]
compactLog = compact [] Nothing Nothing . sortedLog
    where
        compact l _ _ [] = l
        compact l _ _ ((Starts g _):xs) = compact l (Just g) Nothing xs
        compact l g _ ((Sleeps t):xs) = compact l g (Just t) xs
        compact l g s ((Wakes t):xs) = compact ((newEntry g s t):l) g Nothing xs
        newEntry (Just g) (Just s) w = CompactLog g (lMinute s) ((lMinute w) - (lMinute s))

guardLog :: [GuardLog] -> [[CompactLog]]
guardLog = groupBy guardCmp . sortBy (comparing guardId) . compactLog
    where
        guardCmp x y = (guardId x) == (guardId y)

maxMinute :: [CompactLog] -> [Int]
maxMinute = maximumBy (comparing length) . group . sort . concatMap minutes
    where
        minutes e = take (sleepDuration e) [(startMinute e)..]

{-
https://adventofcode.com/2018/day/4

For example, consider the following records, which have already been organized into chronological order:

[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up

Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?

In the example above, Guard #10 spent the most minutes asleep, a total of 50 minutes (20+25+5), while Guard #99 only slept for a total of 30 minutes (10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas any other minute the guard was asleep was only seen on one day).

While this example listed the entries in chronological order, your entries are in the order you found them. You'll need to organize them before they can be analyzed.

What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 10 * 24 = 240.)

--- Part Two ---
Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?

In the example above, Guard #99 spent minute 45 asleep more than any other guard or minute - three times in total. (In all other cases, any guard spent any minute asleep at most twice.)

What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 99 * 45 = 4455.)
-}