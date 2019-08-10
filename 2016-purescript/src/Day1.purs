module Day1 where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Either (Either(..), fromRight)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List.Types (List)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.StringParser
import Text.Parsing.StringParser.CodeUnits
import Text.Parsing.StringParser.Combinators

data Direction = North
               | East
               | South
               | West

instance showDirection :: Show Direction where
  show North = "N"
  show East = "E"
  show South = "S"
  show West = "W"

data Rotation = L | R

instance showRotation :: Show Rotation where
  show L = "L"
  show R = "R"

rotate :: Rotation -> Direction -> Direction
rotate L North = West
rotate L East = North
rotate L South = East
rotate L West = South
rotate R North = East
rotate R East = South
rotate R South = West
rotate R West = North

rotatePoint :: Rotation -> Point -> Point
rotatePoint r (Point d x y) = Point (rotate r d) x y

travel :: Int -> Point -> Point
travel dx (Point East x y)  = Point East  (x + dx) y
travel dx (Point West x y)  = Point West  (x - dx) y
travel dy (Point North x y) = Point North x        (y + dy)
travel dy (Point South x y) = Point South x        (y - dy)

data Point = Point Direction Int Int

instance showPoint :: Show Point where
  show (Point d x y) = (show d) <> "[" <> (show x) <> "," <> (show y) <> "]"

initialPoint :: Point
initialPoint = Point North 0 0

runTransforms :: Array (Point -> Point) -> Point -> Point
runTransforms xforms point = foldl (\p f -> f p) point xforms

digit :: Parser Int
digit = string "0" $> 0
    <|> string "1" $> 1
    <|> string "2" $> 2
    <|> string "3" $> 3
    <|> string "4" $> 4
    <|> string "5" $> 5
    <|> string "6" $> 6
    <|> string "7" $> 7
    <|> string "8" $> 8
    <|> string "9" $> 9

parseRotation :: Parser Rotation
parseRotation = string "L" $> L <|>
                string "R" $> R <|>
                anyChar $> L

data Transform = Transform Rotation Int

instance showTransform :: Show Transform where
  show (Transform rotation distance) = show rotation <> show distance

parseTransform :: Parser Transform
parseTransform = do
  _ <- whiteSpace
  rotation <- parseRotation
  _ <- whiteSpace
  dist <- many1 digit
  _ <- whiteSpace
  pure (Transform rotation $ fromMaybe 0 $ fromString (foldl (\s d -> s <> show d) "" dist))

parseTransforms :: Parser (List Transform)
parseTransforms = parseTransform `sepBy` (string ",")

rights :: Either ParseError (List Transform) -> Array Transform
rights (Left error) = []
rights (Right s) = fromFoldable s

day1 :: Effect Unit
day1 = do
  -- let point = runTransforms xforms initialPoint
  let rawTransforms = rights $ runParser parseTransforms inputData
  log $ show $ rawTransforms

inputData :: String
inputData = "R2, L3, R2, R4, L2, L1, R2, R4, R1, L4, L5, R5, R5, R2, R2, R1, L2, L3, L2, L1, R3, L5, R187, R1, R4, L1, R5, L3, L4, R50, L4, R2, R70, L3, L2, R4, R3, R194, L3, L4, L4, L3, L4, R4, R5, L1, L5, L4, R1, L2, R4, L5, L3, R4, L5, L5, R5, R3, R5, L2, L4, R4, L1, R3, R1, L1, L2, R2, R2, L3, R3, R2, R5, R2, R5, L3, R2, L5, R1, R2, R2, L4, L5, L1, L4, R4, R3, R1, R2, L1, L2, R4, R5, L2, R3, L4, L5, L5, L4, R4, L2, R1, R1, L2, L3, L2, R2, L4, R3, R2, L1, L3, L2, L4, L4, R2, L3, L3, R2, L4, L3, R4, R3, L2, L1, L4, R4, R2, L4, L4, L5, L1, R2, L5, L2, L3, R2, L2"
