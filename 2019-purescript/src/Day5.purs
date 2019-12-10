module Advent.Day5 where

import Prelude

import Advent.Prelude (Effect, always, logShow, parseInt, runDay_, withDelim, (/\))
import Data.Array (catMaybes)

parser :: Array String -> Array Int
parser = catMaybes <<< map parseInt

main :: Effect Unit 
main = do
  withDelim "," "data/day5.txt" $
    runDay_ 5 parser
      $  identity
      /\ always ""
