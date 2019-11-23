module Day6 where

import Prelude

import Data.Array ((..), filter)
import Data.List (List(..), (:), fromFoldable)
import Data.Map (Map, empty, insertWith, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Matrix (Matrix, get, getColumn, fromArray, set, width)
import Matrix as Matrix
import Node.FS.Sync (readTextFile)

type Count = Int

type Histogram a = Map a Count

charsToHisto :: List String -> Histogram String
charsToHisto Nil    = empty
charsToHisto (c:cs) = insertWith (+) c 1 (charsToHisto cs)

-- transpose :: forall a. Matrix a -> Matrix a
-- transpose m = id

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

splitChars :: String -> Array String
splitChars = split (Pattern "")

splitLines :: String -> Array String
splitLines = split (Pattern "\n")

getAllColumns :: forall a. Matrix a -> Array (Array a)
getAllColumns m = fromMaybe [] <$> map ((flip getColumn) m) (0 .. width m)

day6 :: Effect Unit
day6 = do
  text <- readTextFile UTF8 "data/day6.txt"
  let words = filter (not isEmpty) $ splitChars <$> splitLines text
  let wordMatrix = fromMaybe Matrix.empty $ fromArray words
  let columns = fromMaybe [] <$> map ((flip getColumn) wordMatrix) (0 .. width wordMatrix)
  let foo = charsToHisto <<< fromFoldable <$> columns
  log $ show $ bar <$> foo
