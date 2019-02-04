module Day3 where

-- WIP
-- The number falls in a square where the
--   bottom-right is the square of an odd number

square :: Num a => a -> a
square x = x * x

maybeInc :: Integral a => a -> a
maybeInc n = if even n then n + 1 else n

nextOddRoot :: (Floating a, RealFrac a, Integral c) => a -> c
nextOddRoot = maybeInc . ceiling . sqrt

part1 :: (Integral c) => c
part1 = nextRoot
  where
    -- input = 325489 :: Double
    -- root = sqrt input
    nextRoot = nextOddRoot (325489 :: Double)
