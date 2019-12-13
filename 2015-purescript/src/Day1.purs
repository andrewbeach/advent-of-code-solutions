module Advent.Day1 where

import Prelude

import Data.Array (findIndex, take)
import Data.Foldable (sum)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (scanl)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

parenValue :: Char -> Int 
parenValue '(' = 1
parenValue ')' = -1
parenValue _   = 0

parenValues :: String → Array Int
parenValues = map parenValue <<< toCharArray

part1 :: Array Int -> Effect Unit 
part1 values = do 
  let floor  = sum values
      result = "Part 1: " <> (show $ floor) <> " floors"
  log result
    
part2 :: Array Int -> Effect Unit 
part2 values = do 
    let floors = scanl (+) 0 values 
        result = findIndex (_ == (-1)) floors
    log $ show $ take 1781 floors
    log $ "Part 2: " <> (show $ map (_ + 1) result)
   
main :: Effect Unit
main = launchAff_ do 
    liftEffect $ log "--- Day 1 ---"
    text <- readTextFile UTF8 "data/day1.txt"
    let values = parenValues text
    liftEffect $ part1 values
    liftEffect $ part2 values

    
