module Advent.Util.Parsing where 

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1)
import Data.Int (round)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Global (isNaN)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodePoints (char)
import Text.Parsing.StringParser.Combinators (sepBy)

foreign import unsafeParseInt :: Fn1 String Number 

parseInt :: String -> Maybe Int 
parseInt input = 
  if isNaN parsedInt 
  then Nothing 
  else Just (round parsedInt)
  where parsedInt = unsafeParseInt input

type Filename = String 

parseLines :: String -> Array String
parseLines = split (Pattern "\n")

withLines :: forall a. Filename -> (Array String -> Effect a) -> Effect Unit
withLines filename f = launchAff_ do 
  text <- readTextFile UTF8 filename 
  _ <- liftEffect $ f $ parseLines text
  pure unit

type Delim = String

parseDelim :: Delim -> String -> Array String 
parseDelim = split <<< Pattern  

withDelim :: forall a. Delim -> Filename -> (Array String -> Effect a) -> Effect Unit
withDelim delim filename f = launchAff_ do 
  text <- readTextFile UTF8 filename 
  _ <- liftEffect $ f $ parseDelim delim text 
  pure unit 

readCharArray :: Filename -> Aff (Array Char)
readCharArray filename = do 
    text <- readTextFile UTF8 filename
    pure $ toCharArray text

withReadCharArray :: forall a. Filename -> (Array Char -> Effect a) -> Effect Unit
withReadCharArray filename f = launchAff_ do
    charArray <- readCharArray filename  
    _ <- liftEffect $ f charArray
    pure unit


withLineParser :: forall a b. Filename -> Parser a -> (List a -> Effect b) -> Effect Unit
withLineParser filename lineParser f = launchAff_ do 
    text <- readTextFile UTF8 filename
    let parser = lineParser `sepBy` (char '\n')
    case runParser parser text of 
         (Left _) -> pure unit
         (Right as) -> do 
              _ <- liftEffect $ f as
              pure unit
