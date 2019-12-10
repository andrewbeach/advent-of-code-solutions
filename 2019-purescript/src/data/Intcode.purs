module Advent.Intcode where 

import Prelude

import Data.Array (updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe)

type Address = Int 

data Instruction 
  = Sum     Address Address Address
  | Product Address Address Address
  | Stop 

instance showInstruction :: Show Instruction where
  show (Sum a b c) = "Sum<" <> show a <> "," <> show b <> "," <> show c <> ">" 
  show (Product a b c) = 
    "Product<" <> show a <> "," <> show b <> "," <> show c <> ">"
  show Stop = "Stop"

parseInstruction :: Address -> Array Int -> Maybe Instruction
parseInstruction addr as = 
  let getIdx idx = fromMaybe 0 $ as !! idx 
      a = getIdx (addr + 1)
      b = getIdx (addr + 2) 
      c = getIdx (addr + 3) 
  in case (as !! addr) of 
          (Just 1)  -> Just $ Sum a b c
          (Just 2)  -> Just $ Product a b c
          (Just 99) -> Just $ Stop
          otherwise -> Nothing 

computeInstruction :: Instruction -> Array Int -> Maybe Int 
computeInstruction (Sum a b c)     as = (+) <$> as !! a <*> as !! b 
computeInstruction (Product a b c) as = (*) <$> as !! a <*> as !! b
computeInstruction Stop            _   = Nothing 

getDestination :: Instruction -> Maybe Address 
getDestination (Sum     _ _ d) = Just d 
getDestination (Product _ _ d) = Just d 
getDestination Stop            = Nothing 

runInstruction :: Instruction -> Array Int -> Maybe (Array Int)
runInstruction Stop as = Just as  
runInstruction inst   as = join $ 
  updateAt <$> (getDestination inst) <*> (computeInstruction inst as) <*> Just as

instLength :: Instruction -> Int 
instLength (Sum _ _ _)     = 4
instLength (Product _ _ _) = 4
instLength Stop            = 1

foldInstructions :: Address -> Array Int -> Array Int
foldInstructions addr as = 
  let minst = parseInstruction addr as
  in case minst of 
    (Just Stop) -> as
    (Just i) -> foldInstructions (addr + (instLength i)) (fromMaybe [] $ runInstruction i as) 
    otherwise   -> as 
