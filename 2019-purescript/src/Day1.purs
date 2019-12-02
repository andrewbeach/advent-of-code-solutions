module Advent.Day1 where 

import Prelude

import Advent.Prelude (Effect, (/\), parseInt, runDay, withLines)
import Data.Array (catMaybes)
import Data.Foldable (sum)
import Data.Int (floor, toNumber)

type Mass = Int 
type Fuel = Int

massToFuel :: Mass -> Fuel 
massToFuel = toNumber >>> (_/3.0) >>> floor >>> (_-2) >>> max 0

massToFuelRec :: Mass -> Fuel
massToFuelRec = go 0  
  where 
    go :: Fuel -> Mass -> Fuel
    go acc m 
      | m <= 0    = acc
      | otherwise = go (acc + df) df 
      where df = massToFuel m

sumFuel :: (Mass -> Fuel) -> Array String -> Fuel 
sumFuel f = map (parseInt >>> map f)
        >>> catMaybes 
        >>> sum 

main :: Effect Unit 
main = withLines "data/day1.txt" $
  runDay 1 
    $  sumFuel massToFuel
    /\ sumFuel massToFuelRec
