module Day18 ( parseAcreage
             -- , parseLandscape
             ) where

-- import Data.Map (Map)
-- import Linear (V2(..))
-- import qualified Data.Map as M

data Fill = Open
             | Trees
             | Lumber
  deriving (Eq, Ord, Show)

-- type Acre = V2 Int
-- type Landscape = Map Acre Fill

parseAcreage :: Char -> Maybe Fill
parseAcreage sym = case sym of
  '.' -> Just Open
  '|' -> Just Trees
  '#' -> Just Lumber
  _   -> Nothing

-- parseLandscape :: IO [String]
-- parseLandscape = do
--   ls <- lines <$> readFile "data/day18.txt"
--   let ws = concatMap words ls
