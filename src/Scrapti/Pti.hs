module Scrapti.Pti where

import Data.Int (Int16)
import Data.Proxy (Proxy (..))
import Scrapti.Binary (DecodeT, Get, decodeGet)
import Scrapti.Wav (Sampled, Wav, decodeSpecificWav, getSampled)

data PtiHeader = PtiHeader
  {
  } deriving stock (Eq, Show)

data Pti = Pti
  { ptiHeader :: !PtiHeader
  , ptiWav :: !(Wav Int16)
  } deriving stock (Eq, Show)

decodePtiHeader :: Monad m => DecodeT m PtiHeader
decodePtiHeader = decodeGet getPtiHeader

getPtiHeader :: Get PtiHeader
getPtiHeader = error "TODO"

decodePti :: Monad m => DecodeT m Pti
decodePti = do
  hd <- decodePtiHeader
  wav <- decodeSpecificWav Proxy
  pure $! Pti hd wav
