module Advent.Day2 where

import Prelude

import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Foldable (minimum, sum)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (anyDigit, char)
import Text.Parsing.StringParser.Combinators (many, sepBy)

data Box = Box Int Int Int

instance showBox :: Show Box where
    show (Box x y z) = "<Box " <> show x <> " " <> show y <> " " <> show z <> ">"

extractBox :: (List Int) -> Box 
extractBox (x:y:z:Nil) = Box x y z
extractBox _           = Box 0 0 0

surfaceArea :: Box -> Int
surfaceArea (Box x y z) = 
    (2 * x * y) + (2 * x * z) + (2 * y * z)

smallestSideArea :: Box -> Int 
smallestSideArea (Box x y z) = fromMaybe 0 $ minimum [x * y, y * z, x * z] 

totalPaperArea :: Box -> Int 
totalPaperArea box = (surfaceArea box) + (smallestSideArea box)  

dimsParser :: Parser (List (List Char)) 
dimsParser = (many anyDigit) `sepBy` (char 'x')

linesParser :: Parser (List (List (List Char))) 
linesParser = dimsParser `sepBy` (char '\n')

parseBoxes :: String -> Either ParseError (List Box) 
parseBoxes = map (map $ extractBox <<< formatDims) <<< runParser linesParser 
    where formatDims = map $ fromMaybe 0 <<< fromString <<< fromCharArray <<< fromFoldable

part1 :: String -> Effect Unit 
part1 text = do
    let boxes = parseBoxes text
        result = map (sum <<< map totalPaperArea) boxes
    log "Part 1: " 
    log $ show result

main :: Effect Unit
main  = launchAff_ do 
    liftEffect $ log "--- Day 2 ---"
    text <- readTextFile UTF8 "data/day2.txt"
    liftEffect $ part1 text


