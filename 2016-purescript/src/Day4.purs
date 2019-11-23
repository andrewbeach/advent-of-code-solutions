module Day4 where

import Prelude

import Data.Array (filter, fromFoldable, sortBy, take)
import Data.Foldable (foldl)
import Data.List (List)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Custom (multiDigit)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (anyLetter, char, whiteSpace)
import Text.Parsing.StringParser.Combinators (choice, many, manyTill, sepBy)

type Letter = { char :: Char, count :: Int }
type SectorId = Int
type Checksum = Array Char

data Room = Room (Array Char) SectorId Checksum
instance showRoom :: Show Room where
  show (Room name sectorId checksum) =
    "Room<" <> show name <> "," <> show sectorId <> "," <> show checksum <> ">"

letterOrdering :: Letter -> Letter -> Ordering
letterOrdering a b | a.count > b.count = LT
                   | a.count < b.count = GT
                   | otherwise = EQ

top5Letters :: Array Letter -> Array Letter
top5Letters = take 5 <<< sortBy letterOrdering

top5Chars :: Array Letter -> Array Char
top5Chars = map _.char <<< top5Letters

letters :: Array Letter
letters = [ { char: 'a', count: 1 }
          , { char: 'b', count: 2 }
          , { char: 'c', count: 3 }
          , { char: 'd', count: 2 }
          , { char: 'e', count: 1 }
          , { char: 'f', count: 5 }
          , { char: 'g', count: 4 }
          , { char: 'h', count: 1 }
          , { char: 'i', count: 5 }
          , { char: 'j', count: 4 }
          ]

parseRoomName :: Parser (List Char)
parseRoomName = many $ choice [ char '-'
                              , anyLetter
                              ]

parseSectorId :: Parser Int
parseSectorId = do
  id <- multiDigit
  pure id

parseChecksum :: Parser (List Char)
parseChecksum = do
  _ <- char '['
  checksum <- many anyLetter
  _ <- char ']'
  pure checksum

parseRoom :: Parser Room
parseRoom = do
  _ <- whiteSpace
  name <- parseRoomName
  id <- parseSectorId
  checksum <- parseChecksum
  _ <- whiteSpace

  let formatRoomName = filter (\c -> c /= '-') <<< fromFoldable
  let formatChecksum = fromFoldable

  pure $ Room (formatRoomName name) id (formatChecksum checksum)

checksumsEqual :: Checksum -> Checksum -> Boolean
checksumsEqual cs1 cs2 = cs1 == cs2

isRoomReal :: Room -> Boolean
isRoomReal (Room name _ checksum) = checksumsEqual checksum $ ['a']

day4 :: Effect Unit
day4 = do
  text <- readTextFile UTF8 "data/day4.txt"
  let rooms = runParser (many parseRoom `sepBy` char '\n') text
  log $ show $ top5Chars letters
  log $ show $ rooms
