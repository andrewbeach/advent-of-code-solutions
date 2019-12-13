module Advent.Prelude 
  ( module Combinator
  , module Effect 
  , module Parsing
  , module Tuple
  , runDay
  , runDay_
  ) where 

import Advent.Util.Combinator (always) as Combinator
import Advent.Util.Parsing (parseInt, withLines') as Parsing
import Data.Tuple (Tuple(..), fst, snd) as Tuple
import Data.Tuple.Nested ((/\)) as Tuple
import Effect (Effect) as Effect
import Effect.Aff (launchAff_) as Effect
import Effect.Class (liftEffect) as Effect
import Effect.Console (log, logShow) as Effect
import Prelude (class Show, Unit, show, ($), (*>), (<>))

logPart :: forall a. (Show a) => Int -> a -> Effect.Effect Unit
logPart n a = Effect.log $ "Part " <> (show n) <> ": " <> (show a) 

runDay :: forall a b i j. Show a => Show b => Int -> Tuple.Tuple (i -> a) (j -> b) -> i -> j -> Effect.Effect Unit
runDay n (Tuple.Tuple runPart1 runPart2) input1 input2  
  =  Effect.log ("--- Day " <> show n <> " ---")
  *> logPart 1 (runPart1 input1)
  *> logPart 2 (runPart2 input2)

runDay_ :: forall a b i. Show a => Show b => Int -> Tuple.Tuple (i -> a) (i -> b) -> i -> Effect.Effect Unit 
runDay_ n t i = runDay n t i i
