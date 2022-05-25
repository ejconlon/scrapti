{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Pti where

import Data.Int (Int16)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Scrapti.Binary (DecodeT, Get, decodeGet, getByteString, getExpect, getWord8)
import Scrapti.Wav (Wav, decodeSpecificWav)

data PtiHeader = PtiHeader
  { phMajVer :: !Word8
  , phMinVer :: !Word8
  } deriving stock (Eq, Show)

data Pti = Pti
  { ptiHeader :: !PtiHeader
  , ptiWav :: !(Wav Int16)
  } deriving stock (Eq, Show)

decodePtiHeader :: Monad m => DecodeT m PtiHeader
decodePtiHeader = decodeGet getPtiHeader

getPtiHeader :: Get PtiHeader
getPtiHeader = do
  getExpect "0-1: header" (getByteString 2) "PT"
  getExpect "2: ?" getWord8 1
  majVer <- getWord8
  minVer <- getWord8
  pure $! PtiHeader
    majVer
    minVer

decodePti :: Monad m => DecodeT m Pti
decodePti = do
  hd <- decodePtiHeader
  wav <- decodeSpecificWav Proxy
  pure $! Pti hd wav
