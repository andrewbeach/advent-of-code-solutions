module Advent.Prelude 
  ( module Combinator
  , module Parsing
  , module E
  , module T
  , logPart
  , runDay
  , runDay_
  ) where

import Advent.Util.Combinator (always) as Combinator
import Advent.Util.Parsing as Parsing
import Data.Tuple (Tuple(..)) as T
import Data.Tuple.Nested ((/\)) as T
import Effect (Effect) as E
import Effect.Console (log, logShow) as E
import Prelude (class Show, Unit, identity, show, ($), (*>), (<>))

logPart :: forall a. (Show a) => Int -> a -> E.Effect Unit
logPart n a = E.log $ "Part " <> (show n) <> ": " <> (show a) 

runDay_ :: forall a b i j. Show a => Show b => 
        Int -> (i -> j) -> T.Tuple (j -> a) (j -> b) -> i -> E.Effect Unit
runDay_ n f (T.Tuple runPart1 runPart2) input 
  =  E.log ("--- Day " <> show n <> " ---")
  *> logPart 1 (runPart1 $ f input)
  *> logPart 2 (runPart2 $ f input) 

runDay :: forall a b i. Show a => Show b => 
          Int -> T.Tuple (i -> a) (i -> b) -> i -> E.Effect Unit
runDay n = runDay_ n identity
