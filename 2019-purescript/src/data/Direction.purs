module Advent.Data.Direction where

import Prelude

import Data.Maybe (Maybe(..))

data Dir 
  = Up 
  | Right
  | Down
  | Left

instance showDir :: Show Dir where
  show = case _ of 
    Up    -> "U"
    Right -> "R"
    Down  -> "D"
    Left  -> "L"

parseDir :: String -> Maybe Dir 
parseDir = case _ of 
  "U"       -> Just Up 
  "R"       -> Just Right 
  "D"       -> Just Down 
  "L"       -> Just Left
  otherwise -> Nothing


data Translation = Trans Dir Int 

instance showTranslation :: Show Translation where
  show (Trans dir i) = "<" <> show dir <> show i <> ">" 
