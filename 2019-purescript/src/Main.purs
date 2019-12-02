module Main where

import Prelude

import Advent.Day1 as Day1
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"
  Day1.main
