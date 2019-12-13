module Advent.Day8 where 

import Prelude

import Advent.AdventM (AdventM(..), runAdventM)
import Advent.Capability.Day (Day(..), Part(..), runDay)
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

main :: AdventM Unit
main = runDay readLines $ Day 
  { id: 8
  , parts: part1 /\ part2
  }
