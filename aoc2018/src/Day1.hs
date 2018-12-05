module Day1 ( part1
            , part2
            ) where

import qualified Data.Set as Set

parseDelta :: String -> Int
parseDelta s = read $ filter (/= '+') s

findRepeatFreq :: [Int] -> Int -> Set.Set Int -> Int
findRepeatFreq [] _ _ = undefined
findRepeatFreq (d:ds) lastF fs
  | Set.member newF fs = newF
  | otherwise = findRepeatFreq ds newF newFs
  where newF = lastF + d
        newFs = Set.insert newF fs

part1 :: IO Int
part1 = do
  deltas <- (map parseDelta . lines) <$> readFile "data/day1.txt"
  return $ sum deltas

part2 :: IO Int
part2 = do
  deltas <- (map parseDelta . lines) <$> readFile "data/day1.txt"
  return $ findRepeatFreq (cycle deltas) 0 Set.empty
