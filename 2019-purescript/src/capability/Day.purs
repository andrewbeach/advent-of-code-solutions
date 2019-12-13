module Advent.Capability.Day where

import Prelude

import Data.Tuple (Tuple)

type Filename = String

data Part a b = Part 
  { parse      :: a -> b
  , run        :: b -> String
  }

type Part_ a = Part a a

data Day a b c = Day 
  { id       :: Int 
  , parts    :: Tuple (Part a b) (Part a c)
  } 

type Day_ a b = Day a b b

class Monad m <= RunDay m where
  runPart :: forall a b. a -> Part a b -> m Unit
  runDay  :: forall a b c. (Filename -> m a) -> Day a b c -> m Unit 
