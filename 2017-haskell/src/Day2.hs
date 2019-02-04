module Day2 where

import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)

parseInts :: String -> [Int]
parseInts = map read . splitOn "\t"

range :: (Num a, Ord a) => [a] -> a
range ns = (maximum ns) - (minimum ns)

evenDivision :: Int -> Int -> Maybe Int
evenDivision a b = case (mod a b) of
  0 -> Just (quot a b)
  _ -> Nothing

evenDivisions :: [Int] -> Int
evenDivisions xs = sum . catMaybes $ mDivs
  where mDivs = [evenDivision a b | a <- xs, b <- xs, a /= b]

parseRows :: String -> [[Int]]
parseRows = map parseInts . lines

part1 :: IO Int
part1 = do
  rows <- parseRows <$> readFile "data/day2.txt"
  return $ sum . map range $ rows

part2 :: IO Int
part2 = do
  rows <- parseRows <$> readFile "data/day2.txt"
  return $ sum . map evenDivisions $ rows
