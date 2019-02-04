module Day4
  ( Condition (..)
  , parseLogs
  , sampleLogs
  , parseConditions
  ) where

-- import Data.Function (on)
import Data.Maybe (catMaybes)
-- import Data.List (sortBy)

sampleGuardLog :: String
sampleGuardLog = "[1518-02-14 23:52] Guard #2939 begins shift"

sampleAsleepLog :: String
sampleAsleepLog = "[1518-02-15 00:00] falls asleep"

sampleAwakeLog :: String
sampleAwakeLog = "[1518-02-19 00:27] wakes up"

sampleLogs :: [String]
sampleLogs = [ sampleGuardLog
             , sampleAsleepLog
             , ""
             , "short string"
             , sampleAwakeLog
             ]

type GuardId = Int
type Minute = Int

data Condition = Guard GuardId
               | Asleep Minute
               | Awake Minute
               deriving Show

type Parsed3Tuple = (String, String, String)

parseCondition :: Parsed3Tuple -> Maybe Condition
parseCondition (time, indicator, maybeId) =
  let guardId = (read . drop 1) maybeId
      minute = (read . drop 3) time in
    case indicator of
      "Guard" -> Just (Guard guardId)
      "falls" -> Just (Asleep minute)
      "wakes" -> Just (Awake minute)
      _ -> Nothing

parseConditions :: [Parsed3Tuple] -> [Condition]
parseConditions = catMaybes . fmap parseCondition


parseLog :: String -> Maybe Parsed3Tuple
parseLog = toTuple . keepRelevantData
  where keepRelevantData = take 3 . drop 1 . words . filter (\c -> c /= ']')
        toTuple (time:indicator:maybeGuardId:_) = Just (time, indicator, maybeGuardId)
        toTuple _ = Nothing

parseLogs :: [String] -> [Parsed3Tuple]
parseLogs = catMaybes . fmap parseLog

-- up adds the minute from total, down subtracts it
--   this is assoc and commutative, so the order doesn't matter

-- a. organize data

-- only care about beginning shift to know the guard on duty for subsequent naps
-- (guard, Date) -> #{minutes napping}

-- (date -> guard; naps)

-- (date, guard), (date, naps), (guard, naps), (guard, naps, totalMinutes)

-- b. find guard with most minutes asleep
-- c. find most commonly slept-through minute

-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M

--
-- (guard, Map Date #{Start|End}, minutes)

-- if guard line
--   add to date:guard map
-- else
--   lookup guard from date
--   add to
