module Advent.Day3 where

import Prelude

import Advent.Data.Direction (Translation(..), parseDir)
import Advent.Data.Point (Point, translate)
import Advent.Prelude (Effect, always, parseInt, runDay_, withLines, (/\))
import Data.Array (catMaybes, take, (!!))
import Data.Maybe (Maybe, fromMaybe)
import Data.Set (Set, fromFoldable, intersection)
import Data.String (Pattern(..), split, splitAt)
import Data.Traversable (scanl)

parseTranslation :: String -> Maybe Translation 
parseTranslation str = Trans <$> parseDir pts.before <*> parseInt pts.after
  where pts = splitAt 1 str

lineParser :: String -> Array Translation
lineParser = split (Pattern ",") 
         >>> map parseTranslation 
         >>> catMaybes

runTranslations :: Array Translation -> Set Point 
runTranslations = fromFoldable <<< scanl translate mempty

part1 :: Array (Array Translation) -> (Set Point)
part1 arr = intersection line1 line2 where 
  foo = map runTranslations arr
  line1 = fromMaybe mempty $ foo !! 0
  line2 = fromMaybe mempty $ foo !! 1

main :: Effect Unit 
main = do 
  withLines "data/day3.txt" $ 
    runDay_ 3 (take 2 <<< map lineParser)
      $  part1
      /\ always ""
      
