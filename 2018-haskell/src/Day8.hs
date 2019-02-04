module Day8
  ( foo
  , part1
  , num
  ) where

-- import Control.Monad (replicateM)
import Text.Read (readMaybe)
import Text.Parsec (many1, digit)
import Text.Parsec.String (Parser)

foo :: Read a => String -> Maybe [a]
foo = traverse readMaybe . words

-- first token is numChildren
-- second token is numMetas

-- get first two tokens as numChildren, numMetas
-- then, recursively run parser on children and metas

-- type NumChildren = Int
-- type NumMetas = Int
-- type Meta = Int

-- data Node = Node NumChildren NumMetas [Node] [Meta]

num :: Parser Integer
num = do
  n <- digit
  return n


part1 :: IO String
part1 = do
  input <- readFile "data/day8.txt"
  return input
