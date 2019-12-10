module Advent.Day4 where 

import Prelude

import Advent.Prelude (Effect, runDay, (/\))
import Data.Foldable (class Foldable, foldr)
import Data.Int (pow)
import Data.List (List(..), dropWhile, filter, length, range, (:))

adjacentsMatch :: forall a. Eq a => List a -> Boolean
adjacentsMatch (x:y:Nil) = x == y 
adjacentsMatch (x:y:z:xs) 
  | (x == y && y == z) = adjacentsMatch $ dropWhile (_ == x) xs
  | (x == y && y /= z) = true
  | otherwise          = adjacentsMatch (y:z:xs) 
adjacentsMatch _ = false  

neverDecreases :: forall a. Ord a => List a -> Boolean 
neverDecreases (x:y:xs) = 
  if x > y then false else neverDecreases (y:xs) 
neverDecreases _ = true

digits :: Int -> Int -> List Int
digits length = go (length - 1)
  where go :: Int -> Int -> List Int
        go (-1) i = Nil
        go n    i = (i / 10 `pow` n) `mod` 10 : (go (n - 1) i)

isValid :: Int -> Boolean 
isValid = digits 6 
      >>> allPreds [adjacentsMatch, neverDecreases] 

allPreds :: forall a f. Foldable f => f (a -> Boolean) -> a -> Boolean 
allPreds fp x = foldr (\p b -> b && (p x)) true fp

type Count = Int 
part1 :: List Int -> Count
part1 = map isValid 
    >>> filter (_ == true) 
    >>> length 

part2 :: List Int -> Count
part2 = part1

main :: Effect Unit 
main = do 
  let inputs = range 146810 612564
  flip (runDay 4) inputs  
      $  part1
      /\ part2 
      
