module Math.Custom where

import Prelude

inc :: Int -> Int
inc = (_ + 1)

dec :: Int -> Int
dec = (_ - 1)

abs :: Int -> Int
abs x | x < 0 = -x
      | otherwise = x
