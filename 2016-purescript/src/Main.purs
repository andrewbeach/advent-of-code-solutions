module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

-- import Day1 (day1)
-- import Day2 (day2)
-- import Day3 (day3)
-- import Day4 (day4)
import Day6 (day6)

logDayTitle :: Int -> Effect Unit
logDayTitle day = log $ "\n || Day " <> show day <> " || \n"

logDayEnd :: Effect Unit
logDayEnd = log "---------------------"

logDayResult :: Effect Unit -> Int -> Effect Unit
logDayResult e n = do
  logDayTitle n
  e
  logDayEnd

main :: Effect Unit
main = do
  -- logDayResult day1 1
  -- logDayResult day2 2
  -- logDayResult day3 3
  -- logDayResult day4 4
  logDayResult day6 6
