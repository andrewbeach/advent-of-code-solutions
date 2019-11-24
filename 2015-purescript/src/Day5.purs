module Advent.Day5 where

import Prelude

import Advent.Util.Parsing (withLines)
import Data.Foldable (find)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.StringParser.CodeUnits (anyLetter)
import Text.Parsing.StringParser.Combinators (many)

parseVowels :: List Char -> List Char
parseVowels = List.filter \c -> Set.member c vowels
    where vowels = Set.fromFoldable ['a', 'e', 'i', 'o', 'u']

adjacentLetters :: List Char -> Set String
adjacentLetters = go Set.empty
    where go :: Set String -> List Char -> Set String
          go pairs (a:b:cs) = go (Set.insert (fromCharArray [a,b]) pairs) (b:cs)
          go pairs _        = pairs

isDoublePair :: String -> Boolean
isDoublePair s = go $ toCharArray s
    where go [a,b] = a == b
          go _     = false

isBadPair :: String -> Boolean
isBadPair s = Set.member s badPairs
    where badPairs = Set.fromFoldable ["ab", "cd", "pq", "xy"]

isNice :: List Char -> Boolean
isNice cs = hasThreeVowels && hasDoublePair && (not hasBadPair)
    where hasThreeVowels = List.length (parseVowels cs) >= 3 
          adjacents      = adjacentLetters cs
          hasDoublePair  = isJust $ find isDoublePair adjacents
          hasBadPair     = isJust $ find isBadPair adjacents

part1 :: Effect Unit 
part1 = withLines "data/day5.txt" (many anyLetter) \charLists -> do
    let niceStrings = List.filter isNice charLists
    let result = List.length niceStrings
    log $ show result
    pure unit

main :: Effect Unit
main = do 
    log "--- Day 5 ---"
    part1
