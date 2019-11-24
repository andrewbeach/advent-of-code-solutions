module Advent.Util.Coordinate 
    ( Coord(..)
    , Dir(..)
    , translate
    ) where

import Prelude 

-- | Representation of an <x,y> coordinate system
-- | using cardinal directions 
data Coord = Coord Int Int

derive instance eqCoord :: Eq Coord
derive instance ordCoord :: Ord Coord

instance showCoord :: Show Coord where
    show (Coord x y) = "<" <> show x <> "," <> show y <> ">"
    
data Dir 
    = North
    | East
    | South
    | West

instance showDir :: Show Dir where
    show North = "N"
    show East  = "E"
    show South = "S"
    show West  = "W"

-- | Adjust a coordinate in a compass direction
translate :: Coord -> Dir -> Coord 
translate (Coord x y) dir = case dir of
    North -> Coord x       (y + 1)
    East  -> Coord (x + 1) y
    South -> Coord x       (y - 1)
    West  -> Coord (x - 1) y

