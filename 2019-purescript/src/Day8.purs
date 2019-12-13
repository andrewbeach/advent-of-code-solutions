module Advent.Day8 where 

import Prelude

import Advent.AdventM (runAdventM)
import Advent.Capability.Day (Day(..), Part(..), Day_, runDay)
import Advent.Capability.Read (readLines)
import Advent.Util.Combinator (always)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)

type Lines = Array String 

part1 :: Part Lines Lines
part1 = Part 
  { parse: identity
  , run: always "undefined"
  }

part2 :: Part Lines Lines 
part2 = Part 
  { parse: identity
  , run: always "undefined"
  }

day8 :: Day_ Lines Lines 
day8 = Day 
  { id: 1
  , parts: part1 /\ part2
  }

main :: Effect Unit 
main = launchAff_ $ runAdventM { filename: "data/day8.txt" } $ 
  runDay readLines day8
