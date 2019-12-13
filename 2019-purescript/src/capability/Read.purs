module Advent.Capability.Read where

import Prelude

import Data.String (Pattern(..), split)

parseLines :: String -> Array String
parseLines = split (Pattern "\n")

type Filename = String

class Monad m <= ReadFile m where
  readLines :: Filename -> m (Array String) 
