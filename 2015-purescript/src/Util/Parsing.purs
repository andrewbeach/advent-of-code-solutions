module Advent.Util.Parsing where

import Prelude

import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

type Filename = String

readCharArray :: Filename -> Aff (Array Char)
readCharArray fn = do 
    text <- readTextFile UTF8 fn
    pure $ toCharArray text

withReadCharArray :: forall a. Filename -> (Array Char -> Effect a) -> Effect Unit
withReadCharArray filename f = launchAff_ do
    charArray <- readCharArray filename  
    _ <- liftEffect $ f charArray
    pure unit
