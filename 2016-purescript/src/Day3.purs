module Day3 where

-- Day 3: Squares with Three Sides

import Prelude

import Data.List (filter, length)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Custom (multiDigit)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (whiteSpace)
import Text.Parsing.StringParser.Combinators (many)

type Length = Int

data Triangle = Triangle Int Int Int
instance showTriangle :: Show Triangle where
  show (Triangle a b c) =
    "Triangle<" <> show a <> "," <> show b <> "," <> show c <> ">"

isValid :: Triangle -> Boolean
isValid (Triangle a b c) =
  (a + b) > c &&
  (a + c) > b &&
  (b + c) > a

parseTriangleHorizontally :: Parser Triangle
parseTriangleHorizontally = do
  _ <- whiteSpace

  a <- multiDigit
  b <- multiDigit
  c <- multiDigit

  pure $ Triangle a b c

part1 :: String -> Effect Unit
part1 text = do
  let triangles = runParser (many parseTriangleHorizontally) text
  let validCount = length <<< filter isValid <$> triangles
  log $ "All triangles: " <> show (length <$> triangles)
  log $ "Valid: " <> show validCount

day3 :: Effect Unit
day3 = do
  text <- readTextFile UTF8 "data/day3.txt"
  part1 text
