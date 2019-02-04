module Day2 ( letterFreq
            , part1
            ) where

import qualified Data.Map.Strict as Map

letterFreq :: String -> Map.Map Char Int
letterFreq s = Map.fromListWith (+) (map (\x -> (x, 1)) s)

filterForFreq :: Int -> Map.Map Char Int -> [Int]
filterForFreq n = (Map.elems . Map.filter (== n))

part1 :: IO ()
part1 = do
  codes <- lines <$> readFile "data/day2.txt"
  let freqs  = map letterFreq codes
      twos   = filter (not . null) $ map (filterForFreq 2) freqs
      threes = filter (not . null) $ map (filterForFreq 3) freqs
  print $ (length twos) * (length threes)
