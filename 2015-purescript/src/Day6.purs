module Advent.Day6 where

import Prelude

import Advent.Util.Parsing (withLines)
import Data.Foldable (class Foldable, foldl, foldr)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.StringParser.CodeUnits (string)
import Text.Parsing.StringParser.Combinators (choice)

type Xform a = a -> a

data Light = Off | On

instance showLight :: Show Light where
    show Off = "Off"
    show On  = "On"

turnOn :: Xform Light
turnOn = const On

turnOff :: Xform Light
turnOff = const Off 

toggle :: Xform Light
toggle On = Off
toggle Off = On

runXforms :: forall f a. Foldable f => f (Xform a) -> a -> a
runXforms xforms a = foldl (\x xform -> xform x) a xforms

part1 :: Effect Unit
part1 = do 
    log $ show $ runXforms [turnOff, toggle, turnOn, toggle] Off
    let parser = choice [ string "toggle"
                        , string "turn on"
                        , string "turn off"
                        ]
    withLines "data/day6.txt" parser \str -> do 
        log "Part 1: "
        log $ show str

main :: Effect Unit
main = do
    log "--- Day 6 ---"
    part1
