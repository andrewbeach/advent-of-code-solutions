module Advent.Util.Parsing where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (char)
import Text.Parsing.StringParser.Combinators (sepBy)

type Filename = String

readCharArray :: Filename -> Aff (Array Char)
readCharArray filename = do 
    text <- readTextFile UTF8 filename
    pure $ toCharArray text

withReadCharArray :: forall a. Filename -> (Array Char -> Effect a) -> Effect Unit
withReadCharArray filename f = launchAff_ do
    charArray <- readCharArray filename  
    _ <- liftEffect $ f charArray
    pure unit

withLines :: forall a b. Filename -> Parser a -> (List a -> Effect b) -> Effect Unit
withLines filename lineParser f = launchAff_ do 
    text <- readTextFile UTF8 filename
    let parser = lineParser `sepBy` (char '\n')
    case runParser parser text of 
         (Left _) -> pure unit
         (Right as) -> do 
              _ <- liftEffect $ f as
              pure unit

