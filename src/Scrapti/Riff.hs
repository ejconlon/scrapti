{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Riff
  ( labelRiff
  , getLabel
  , expectLabel
  , getChunkSize
  , expectChunkSize
  , putChunkSize
  ) where

import Data.ByteString (ByteString)
import Scrapti.Binary (Get, getByteString, getExpect, ByteLength, Put, Word32LE, Binary (..))

labelRiff :: ByteString
labelRiff = "RIFF"

getLabel :: Get ByteString
getLabel = getByteString 4

expectLabel :: ByteString -> Get ()
expectLabel = getExpect "label" getLabel

getChunkSize :: Get ByteLength
getChunkSize = fmap fromIntegral (get @Word32LE)

expectChunkSize :: ByteLength -> Get ()
expectChunkSize = getExpect "chunk size" getChunkSize

putChunkSize :: ByteLength -> Put
putChunkSize = put @Word32LE . fromIntegral
