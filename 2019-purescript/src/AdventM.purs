module Advent.AdventM where 

import Prelude

import Advent.Capability.Day (class RunDay, Day(..), Part(..), runPart)
import Advent.Capability.Log (class CanLog, log)
import Advent.Capability.Read (class ReadFile)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), split)
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Type.Equality as TE

type Env = { filename :: String }

newtype AdventM a = AdventM (ReaderT Env Aff a)

derive         instance newtypeAdventM     :: Newtype (AdventM a) _
derive newtype instance functorAdventM     :: Functor AdventM
derive newtype instance applyAdventM       :: Apply AdventM
derive newtype instance applicativeAdventM :: Applicative AdventM 
derive newtype instance bindAdventM        :: Bind AdventM
derive newtype instance monadAdventM       :: Monad AdventM 
derive newtype instance monadEffectAdventM :: MonadEffect AdventM
derive newtype instance monadAffAdventM    :: MonadAff AdventM

instance monadAskAdventM :: TE.TypeEquals e Env => MonadAsk e AdventM where
  ask = AdventM $ asks TE.from

instance readFileAdventM :: ReadFile AdventM where
  readLines filename = liftAff $ parseLines <$> readTextFile UTF8 filename
    where parseLines = split (Pattern "\n")
  
instance canLogAdventM :: CanLog AdventM where
  log     = liftEffect <<< Console.log
  logShow = liftEffect <<< Console.logShow

instance runPartAdventM :: RunDay AdventM where
  runPart input (Part { parse, run }) = 
    input # parse # run # log 

  runDay reader (Day { id, parts }) = do 
     log $ "--- Day " <> show id <> " ---"
     env <- ask
     input <- reader env.filename
     log "Part 1"
     runPart input (fst parts)
     let rule = "-------------"
     log rule
     log "Part 2"
     runPart input (snd parts)
     log rule

runAdventM :: Env -> AdventM ~> Aff
runAdventM e (AdventM m) = runReaderT m e
