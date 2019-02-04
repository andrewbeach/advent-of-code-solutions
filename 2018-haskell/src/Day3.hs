module Day3 where
-- module Day3
--   ( coordsToString
--   , dimsToCoords
--   , part1
--   ) where

-- import qualified Data.Map as M

-- type XInit = String
-- type YInit = String
-- type Width = String
-- type Height = String

-- coordsToString :: (Int, Int) -> String
-- coordsToString (x,y) = (show x) ++ ":" ++ (show y)

-- dimsToCoords :: (XInit, YInit, Width, Height) -> [(Int, Int)]
-- dimsToCoords (x0,y0,dx,dy) =
--   [(x,y) | x <- [(read x0)..((read x0) + (read dx))]
--          , y <- [(read y0)..((read y0) + (read dy))]]

-- claimToDims :: String -> (XInit, YInit, Width, Height)
-- claimToDims c = undefined

-- lineToDims :: String -> (XInit, YInit, Width, Height)
-- lineToDims l = read l :: (XInit, YInit, Width, Height)

-- part1 :: IO ()
-- part1 = do
--   dims <- (fmap (\l -> read l) . lines) <$> readFile "data/day3.txt"
--   print $ take 10 dims

-- rectHistogram :: XInit -> YInit -> Width -> Height -> M.Map String Int
-- rectHistogram x0 y0 dx dy = [(x,y) | x <-

-- claimToMap :: Claim -> M.Map String Int
-- claimToMap c = M.fromList [(coordsToString (x0 c)   , 1)]
