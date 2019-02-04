module Day4 where

import Data.List (sort)

allUnique :: (Eq a) => [a] -> Bool
allUnique = go []
  where go _ [] = True
        go seen (x:xs)
          | x `elem` seen = False
          | otherwise = go (seen ++ [x]) xs

countValid :: [[String]] -> Int
countValid = length . filter id . map allUnique

getPhrases :: IO [[String]]
getPhrases = map words . lines <$> readFile "data/day4.txt"

part1 :: IO Int
part1 = countValid <$> getPhrases

part2 :: IO Int
part2 = countValid <$> map (map sort) <$> getPhrases
