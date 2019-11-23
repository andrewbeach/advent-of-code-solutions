module Externs (Radix, toRadix, parseInt) where

import Prelude ((<), (>))
import Data.Maybe (Maybe (..))
import Data.Int (round)
import Data.Function.Uncurried (Fn2, runFn2)
import Global (isNaN)


foreign import unsafeParseInt :: Fn2 String Int Number

newtype Radix = Radix Int

-- | Enforces the base to be between `2` and `36`
toRadix :: Int -> Radix
toRadix x = Radix (if x < 2 then 2 else if x > 36 then 36 else x)

-- | Warning - this function follows the same semantics as native JS's `parseInt()` function -
-- | it will parse "as much as it can", when it can - sometimes it succeeds when the input isn't
-- | completely sanitary.
parseInt :: String -> Radix -> Maybe Int
parseInt s (Radix r) =
  let x = runFn2 unsafeParseInt s r
  in  if isNaN x
      then Nothing
      else Just (round x)
