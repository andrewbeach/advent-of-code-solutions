module Advent.Day3 where

import Prelude

import Advent.Util.Coordinate (Coord(..), Dir(..), translate)
import Advent.Util.Parsing (withReadCharArray)
import Data.Foldable (foldr)
import Data.Int (even)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Traversable (scanl)
import Data.TraversableWithIndex (scanlWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

parseDir :: Char -> Maybe Dir 
parseDir c  
    | c == '^'  = Just North
    | c == '>'  = Just East
    | c == 'v'  = Just South
    | c == '<'  = Just West
    | otherwise = Nothing

-- | Scan over directions, starting from origin, to list visited coordinates
-- | Then, use set insertion to count unique visits
part1 :: Array Dir -> Effect Unit
part1 directions = do 
    let coords = scanl translate (Coord 0 0) directions
        hitLog = foldr Set.insert (Set.singleton (Coord 0 0)) coords
        result = 
            "Part 1: " <> 
            show (Set.size hitLog) <> 
            " houses receive at least one present" 
    log result

-- | Scan over directions, alternating, to track visits by santa & robo-santa
-- | Then, use set insertion to count unique visits  
part2 :: Array Dir -> Effect Unit
part2 directions = do
    let initCoord = Coord 0 0
        coords = scanlWithIndex 
                 (\idx (Tuple santa robo) dir -> 
                     if even idx
                     then Tuple (translate santa dir) robo
                     else Tuple santa                 (translate robo dir)
                 ) 
                 (Tuple initCoord initCoord) 
                 directions
        hitLog = foldr 
                 (\t hits -> Set.insert (fst t) $ Set.insert (snd t) hits) 
                 Set.empty 
                 coords 
        result = 
            "Part 2: " <> 
            show (Set.size hitLog) <>
            " houses receive at least one present"
    log result

main :: Effect Unit
main = withReadCharArray "data/day3.txt" \cs -> do
    liftEffect $ log "--- Day 3 ---"
    let directions = map (fromMaybe North <<< parseDir) cs
    part1 directions
    part2 directions 
