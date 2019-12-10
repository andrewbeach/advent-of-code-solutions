module Advent.Data.Point where 

import Prelude

import Advent.Data.Direction (Dir(..), Translation(..))

data Point = Point Int Int

instance showPoint :: Show Point where
  show (Point x y) = "(" <> show x <> "," <> show y <> ")"

instance eqPoint :: Eq Point where
  eq (Point x1 y1) (Point x2 y2) = x1 == x2 && y1 == y2

instance ordPoint :: Ord Point where
  compare (Point x1 y1) (Point x2 y2) = 
    if x1 == x2 then compare y1 y2 else compare x1 x2

instance semigroupPoint :: Semigroup Point where
  append (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

instance monoidPoint :: Monoid Point where
  mempty = Point 0 0

translate :: Point -> Translation -> Point 
translate (Point x y) (Trans dir dist) = case dir of 
  Up    -> Point x          (y + dist)
  Right -> Point (x + dist) y 
  Down  -> Point x          (y - dist) 
  Left  -> Point (x - dist) y
