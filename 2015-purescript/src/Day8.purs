module Advent.Day8 where 

import Prelude

import Advent.Prelude (Effect, always, log, runDay_, withLines', (/\))
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)

data EscapeSeq 
  = Backslash
  | DoubleQuote
  | Hex

instance showEscapeSeq :: Show EscapeSeq where
  show Backslash = "B"
  show DoubleQuote = "Q"
  show Hex = "H"
 
strToCharList :: String -> List Char 
strToCharList = List.fromFoldable <<< toCharArray 

charListToStr :: List Char -> String 
charListToStr = fromCharArray <<< Array.fromFoldable

unescape :: String -> String 
unescape = replace (unsafeRegex "\\\\" global) "B"
        >>> replace (unsafeRegex "\\x\\d{2}" global) "H"  
 
main :: Effect Unit 
main = do 
  withLines' "data/day8.txt" $ 
    runDay_ 8 
      $  always ""
      /\ always ""
