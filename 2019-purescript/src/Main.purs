module Main where

import Prelude

import Advent.AdventM (runAdventM)
import Advent.Day8 as Day8
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"
  launchAff_ $ runAdventM { filename: "data/day8.txt" } Day8.main
