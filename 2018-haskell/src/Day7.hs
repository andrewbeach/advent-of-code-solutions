module Day7 (foo) where

import qualified Data.Map as M

foo :: IO (M.Map String String)
foo = go <$> readFile "data/day7_parsed.txt"
  where go = (M.fromList . fmap toPairs . fmap words . lines)
        toPairs p = case p of
          (x:y:_) -> (x,y)
          _ -> ("","")
