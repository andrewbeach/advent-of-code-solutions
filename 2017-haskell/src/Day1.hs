module Day1 where

import Data.Char (digitToInt)

sample :: [Int]
sample = [1, 2, 2, 1]

sumAdjacentMatches :: (Num a, Eq a) => a -> [a] -> a
sumAdjacentMatches total [] = total
sumAdjacentMatches total (_:[]) = total
sumAdjacentMatches total (x1:x2:xs) = sumAdjacentMatches total' (x2:xs)
  where total' = if (x1 == x2) then total + x1 else total

sumOppositeMatches :: (Num a, Eq a) => [a] -> [a] -> [a]
sumOppositeMatches = zipWith $
  \a ->
    \b -> if (a == b) then (a + b) else 0

readDigits :: IO [Int]
readDigits = do
  captcha <- readFile "data/day1.txt"
  return $ map digitToInt $ filter (/= '\n') captcha

part1 :: IO Int
part1 = do
  digits <- readDigits
  let wrapped = take (length digits + 1) $ cycle digits
  return $ sumAdjacentMatches 0 wrapped

part2 :: IO Int
part2 = do
  digits <- readDigits
  let k = quot (length digits) 2
  let left = take k digits
  let right = drop k digits
  return $ sum $ sumOppositeMatches left right
