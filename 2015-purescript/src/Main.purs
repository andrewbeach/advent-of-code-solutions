module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello World!"

foo :: Either String Int -> String 
foo (Left x) = x
foo (Right _) = "no string found"


