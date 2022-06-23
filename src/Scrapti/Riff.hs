{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Riff
  ( Label
  , labelRiff
  , expectLabel
  , getChunkSize
  , expectChunkSize
  , putChunkSize
  ) where

import Scrapti.Binary (Binary (..), ByteLength, FixedBytes, Get, Put, Word32LE, getExpect)

type Label = FixedBytes 4

labelRiff :: Label
labelRiff = "RIFF"

expectLabel :: Label -> Get ()
expectLabel = getExpect "label" get

getChunkSize :: Get ByteLength
getChunkSize = fmap fromIntegral (get @Word32LE)

expectChunkSize :: ByteLength -> Get ()
expectChunkSize = getExpect "chunk size" getChunkSize

putChunkSize :: ByteLength -> Put
putChunkSize = put @Word32LE . fromIntegral
