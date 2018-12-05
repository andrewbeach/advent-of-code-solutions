module Day1 ( part1
            , part2
            ) where

import qualified Data.IntSet as IntSet
import Data.Maybe (Maybe(Just, Nothing))

parseDelta :: String -> Int
parseDelta s = read $ filter (/= '+') s

findRepeatFreq :: [Int] -> Int -> IntSet.IntSet -> Maybe Int
findRepeatFreq [] _ _ = Nothing
findRepeatFreq (d:ds) lastF fs
  | IntSet.member newF fs = Just newF
  | otherwise = findRepeatFreq ds newF newFs
  where newF = lastF + d
        newFs = IntSet.insert newF fs

part1 :: IO Int
part1 = do
  deltas <- (map parseDelta . lines) <$> readFile "data/day1.txt"
  return $ sum deltas

part2 :: IO Int
part2 = do
  deltas <- (map parseDelta . lines) <$> readFile "data/day1.txt"
  maybeFreq <- return $ findRepeatFreq (cycle deltas) 0 IntSet.empty
  return $ maybe 0 id maybeFreq
