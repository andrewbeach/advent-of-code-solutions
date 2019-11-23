module Text.Parsing.Custom where

import Prelude

import Data.Array (fromFoldable)
import Data.List (List)
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Externs (parseInt, toRadix)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (anyDigit, satisfy, whiteSpace)
import Text.Parsing.StringParser.Combinators (many, many1, sepEndBy)

lineBreak :: Parser Char
lineBreak = satisfy \c -> c == '\n'

lineParser :: Parser (List Char)
lineParser = many (satisfy \c -> c /= '\n')

linesParser :: Parser (List (List Char))
linesParser = lineParser `sepEndBy` lineBreak

charListToInt :: List Char -> Int
charListToInt cs = fromMaybe $ parseInt joinedChars (toRadix 10)
  where
    joinedChars :: String
    joinedChars = fromCharArray <<< fromFoldable $ cs
    fromMaybe :: Maybe Int -> Int
    fromMaybe Nothing = 0
    fromMaybe (Just i) = i

multiDigit :: Parser Int
multiDigit = do
  d <- many1 anyDigit
  _ <- whiteSpace
  pure $ charListToInt (toList d)
