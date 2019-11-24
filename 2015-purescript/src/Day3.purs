module Advent.Day3 where

import Prelude

import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (even)
import Data.Set as Set
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (scanl)
import Data.TraversableWithIndex (scanlWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

data Dir 
    = North
    | East
    | South
    | West

instance showDir :: Show Dir where
    show North = "N"
    show East  = "E"
    show South = "S"
    show West  = "W"

parseDir :: Char -> Dir 
parseDir c  
    | c == '^'  = North
    | c == '>'  = East
    | c == 'v'  = South
    | c == '<'  = West
    | otherwise = North

data Coord = Coord Int Int

instance showCoord :: Show Coord where
    show (Coord x y) = "<" <> show x <> "," <> show y <> ">"

derive instance eqCoord :: Eq Coord
derive instance ordCoord :: Ord Coord

type HitLog = Set.Set Coord

moveCoord :: Coord -> Dir -> Coord 
moveCoord (Coord x y) dir = case dir of
    North -> Coord x       (y + 1)
    East  -> Coord (x + 1) y
    South -> Coord x       (y - 1)
    West  -> Coord (x - 1) y

part1 :: Array Dir -> Effect Unit
part1 directions = do 
    let coords = scanl moveCoord (Coord 0 0) directions
        hitLog = foldr Set.insert (Set.singleton (Coord 0 0)) coords
        result = 
            "Part 1: " <> 
            show (Set.size hitLog) <> 
            " houses receive at least one present" 
    log result

part2 :: Array Dir -> Effect Unit
part2 directions = do
    let initCoord = Coord 0 0
        coords = scanlWithIndex 
                 (\idx (Tuple santa robo) dir -> 
                     if even idx
                     then Tuple (moveCoord santa dir) robo
                     else Tuple santa                 (moveCoord robo dir)
                 ) 
                 (Tuple initCoord initCoord) 
                 directions
        hitLog = foldr (\t hits -> Set.insert (fst t) $ Set.insert (snd t) hits) 
                       Set.empty 
                       coords 
        result = 
            "Part 2: " <> 
            show (Set.size hitLog) <>
            " houses receive at least one present"
    log result

main :: Effect Unit
main = launchAff_ do 
    liftEffect $ log "--- Day 3 ---"
    text <- readTextFile UTF8 "data/day3.txt"
    let directions = map parseDir <<< toCharArray $ text
    liftEffect $ part1 directions
    liftEffect $ part2 directions 
