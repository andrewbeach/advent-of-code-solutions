module Advent.Day2 where 

import Prelude

import Advent.Intcode (foldInstructions)
import Advent.Prelude (Effect, always, parseInt, runDay, withDelim, (/\))
import Data.Array (catMaybes, updateAt)
import Data.Maybe (Maybe)

part1 :: Array String -> Maybe (Array Int)
part1 vals = 
  let codes = catMaybes $ map parseInt vals
      noun = 76
      verb = 10
      initializedCodes :: Maybe (Array Int) 
      initializedCodes = join $ updateAt 2 verb <$> updateAt 1 noun codes
  in foldInstructions 0 <$> initializedCodes

main :: Effect Unit 
main = withDelim "," "data/day2.txt" $ do   
  runDay 2
    $  part1 
    /\ always ""
