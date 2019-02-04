{-# OPTIONS_GHC -Wunused-do-bind  #-}

module Day10 where

import Text.ParserCombinators.ReadP

-- import qualified Data.Map as M

data Position = Position Int Int
  deriving (Eq, Show)
data Velocity = Velocity Int Int
  deriving (Eq, Show)

isVowel :: Char -> Bool
isVowel c = any (c ==) "aeiou"

vowel :: ReadP Char
vowel = satisfy isVowel

letter :: ReadP Char
letter = satisfy (\c -> c >= 'A' && c <= 'z')

digitOrNegative :: ReadP Char
digitOrNegative =
  satisfy (\c -> c == '-' || (c >= '0' && c <= '9'))

-- learning what you can do with just Text.ParserCombinators
coord :: ReadP Position
coord = do
  _ <- many1 letter
  _ <- many1 $ satisfy (\c -> c == '=' || c == '<')
  x <- many1 digitOrNegative
  _ <- many1 $ satisfy (\c -> c == ',' || c == ' ')
  y <- many1 digitOrNegative
  _ <- satisfy (== '>')
  return $ Position (read x) (read y)
