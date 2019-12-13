module Advent.Capability.Log where

import Prelude 

class Monad m <= CanLog m where 
  log :: String -> m Unit 
  logShow :: forall a. Show a => a -> m Unit 
