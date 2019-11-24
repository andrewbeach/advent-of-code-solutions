module Advent.Day4 where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Crypto.Hash (Algorithm(..), hex)

startsWithNZeroes :: Int -> List Char -> Boolean
startsWithNZeroes _ Nil      = false
startsWithNZeroes _ (_:Nil) = false
startsWithNZeroes n ('0':xs)  = 
    if n == 1 
    then true 
    else startsWithNZeroes (n - 1) xs
startsWithNZeroes _ _       = false

findPostfix :: Int -> String -> Int -> Effect (Tuple Int String)
findPostfix numZeroes prefix guess = do 
    hash <- hex MD5 $ prefix <> show guess 
    if startsWithNZeroes numZeroes $ List.fromFoldable $ toCharArray hash
    then pure $ Tuple guess hash
    else findPostfix numZeroes prefix (guess + 1)

part1 :: Effect Unit
part1 = do 
    let input = "iwrupvqb"
    result <- findPostfix 5 input 0
    log $ "Part 1: " <> show result

part2 :: Effect Unit 
part2 = do 
    let input = "iwrupvqb"
    result <- findPostfix 6 input 0
    log $ "Part 2: " <> show result
    
main :: Effect Unit
main = do
    log "--- Day 4 ---"
    part1
    part2
