module Day2 ( letterFreq
            , part1
            ) where

import qualified Data.Map.Strict as Map
-- import qualified Data.IntSet as IntSet

letterFreq :: String -> Map.Map Char Int
letterFreq s = Map.fromListWith (+) (map (\x -> (x, 1)) s)

filterForFreq :: Int -> Map.Map Char Int -> [Int]
filterForFreq n = (Map.elems . Map.filter (== n))

nonEmpty :: [Int] -> Bool
nonEmpty [] = False
nonEmpty _ = True

part1 :: IO Int
part1 = do
  codes <- lines <$> readFile "data/day2.txt"
  freqs <- (return . map letterFreq) codes
  twos <- return $ filter nonEmpty $ map (filterForFreq 2) freqs
  threes <- return $ filter nonEmpty $ map (filterForFreq 3) freqs
  return $ (length twos) * (length threes)
