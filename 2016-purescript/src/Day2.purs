module Day2 where

import Prelude

import Data.Either as E
import Data.Foldable (foldl)
import Data.List (List(..), (:), snoc)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (log)
import Math.Custom (abs, dec, inc)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Custom (linesParser)
import Text.Parsing.StringParser (runParser, ParseError)

data Pair = Pair Int Int

instance showPair :: Show Pair where
  show (Pair x y) = "[" <> show x <> "," <> show y <> "]"

type Button = Pair
type Focus = Pair

-- TODO: make these visual grids with coordinates for better maintenance
standardFocusValue :: Focus -> Char
standardFocusValue f = case f of
  (Pair -1  1) -> '1'
  (Pair  0  1) -> '2'
  (Pair  1  1) -> '3'
  (Pair -1  0) -> '4'
  (Pair  0  0) -> '5'
  (Pair  1  0) -> '6'
  (Pair -1 -1) -> '7'
  (Pair  0 -1) -> '8'
  (Pair  1 -1) -> '9'
  (Pair  _  _) -> '0'

altFocusValue :: Focus -> Char
altFocusValue f = case f of
  (Pair  0  2) -> '1'
  (Pair -1  1) -> '2'
  (Pair  0  1) -> '3'
  (Pair  1  1) -> '4'
  (Pair -2  0) -> '5'
  (Pair -1  0) -> '6'
  (Pair  0  0) -> '7'
  (Pair  1  0) -> '8'
  (Pair  2  0) -> '9'
  (Pair -1 -1) -> 'A'
  (Pair  0 -1) -> 'B'
  (Pair  1 -1) -> 'C'
  (Pair  0 -2) -> 'D'
  (Pair  _  _) -> '0'

data Direction
  = Up
  | Left
  | Down
  | Right

-- Why can't this be derived automatically?
instance showDirection :: Show Direction where
  show Up    = "Up"
  show Left  = "Left"
  show Down  = "Down"
  show Right = "Right"

directionFromChar :: Char -> Maybe Direction
directionFromChar c = case c of
  'U' -> Just Up
  'L' -> Just Left
  'D' -> Just Down
  'R' -> Just Right
  _   -> Nothing

directionsFromChars :: List Char -> List (Maybe Direction)
directionsFromChars cs = directionFromChar <$> cs

-- TODO can these be abstracted and combined?
translateFocus :: Focus -> Direction -> Focus
translateFocus (Pair x y) dir = case dir of
  Up    -> Pair x                  (min (inc y) 1)
  Down  -> Pair x                  (max (dec y) (-1))
  Right -> Pair (min (inc x) 1)    y
  Left  -> Pair (max (dec x) (-1)) y

altTranslateFocus :: Focus -> Direction -> Focus
altTranslateFocus (Pair x y) dir = case dir of
  Up    -> if abs x + abs (inc y) > 2 then Pair x y else Pair x (inc y)
  Down  -> if abs x + abs (dec y) > 2 then Pair x y else Pair x (dec y)
  Right -> if abs (inc x) + abs y > 2 then Pair x y else Pair (inc x) y
  Left  -> if abs (dec x) + abs y > 2 then Pair x y else Pair (dec x) y

-- TODO rename
handleEither :: E.Either ParseError (List (List Char)) -> List (Maybe (List Direction))
handleEither e = case e of
  (E.Left _) -> Nil
  (E.Right charLists) -> sequence <$> directionsFromChars <$> charLists

parseDirections :: String -> List (Maybe (List Direction))
parseDirections lines = handleEither $ runParser linesParser lines

-- TODO abstract and reuse a fn for both parts
part1 :: List Char -> Focus -> List (Maybe (List Direction)) -> Maybe (List Char)
part1 combo _     Nil             = Just combo
part1 combo _     (Just(Nil):Nil) = Just combo
part1 combo focus (dir:dirs)      = do
  newFocus <- foldl translateFocus focus <$> dir
  let newCombo = combo `snoc` standardFocusValue newFocus
  part1 newCombo newFocus dirs

part2 :: List Char -> Focus -> List (Maybe (List Direction)) -> Maybe (List Char)
part2 combo _     Nil             = Just combo
part2 combo _     (Just(Nil):Nil) = Just combo
part2 combo focus (dir:dirs)      = do
  newFocus <- foldl altTranslateFocus focus <$> dir
  let newCombo = combo `snoc` altFocusValue newFocus
  part2 newCombo newFocus dirs

day2 :: Effect Unit
day2 = do
  text <- readTextFile UTF8 "data/day2.txt"
  let directions = parseDirections text

  let initialFocus1 = Pair 0 0
  let answer1 = part1 Nil initialFocus1 directions
  log $ show $ answer1

  let initialFocus2 = Pair (-2) 0
  let answer2 = part2 Nil initialFocus2 directions
  log $ show $ answer2
