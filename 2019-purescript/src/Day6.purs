module Advent.Day6 where

import Prelude

import Advent.Prelude (Effect, always, logShow, runDay, withLines, (/\))
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set

type Obj = String

dict :: Map Obj Obj
dict = Map.empty 
     # Map.insert "ABC" "DEF"

main :: Effect Unit
main = do 
  let used = Set.empty
  logShow dict
  withLines "data/day6.txt" $
    runDay 6 
    $  always ""
    /\ always ""
