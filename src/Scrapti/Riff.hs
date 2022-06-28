{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Riff
  ( Label
  , labelRiff
  , getExpectLabel
  , getChunkSize
  , expectChunkSize
  , putChunkSize
  , chunkHeaderSize
  , StaticLabel (..)
  , Chunk (..)
  ) where

import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, Proxy (..), Put, StaticByteSized (..), StaticBytes,
               Word32LE, getExact, getExpect)
import Data.Default (Default)
-- import Debug.Trace (traceM)

type Label = StaticBytes 4

labelRiff :: Label
labelRiff = "RIFF"

getExpectLabel :: Label -> Get ()
getExpectLabel = getExpect "label" get

getChunkSize :: Get ByteCount
getChunkSize = fmap fromIntegral (get @Word32LE)

expectChunkSize :: ByteCount -> Get ()
expectChunkSize = getExpect "chunk size" getChunkSize

putChunkSize :: ByteCount -> Put
putChunkSize = put @Word32LE . fromIntegral

chunkHeaderSize :: ByteCount
chunkHeaderSize = 8

class StaticLabel a where
  staticLabel :: Proxy a -> Label

newtype Chunk a = Chunk { chunkValue :: a }
  deriving stock (Show)
  deriving newtype (Eq, Default)

instance ByteSized a => ByteSized (Chunk a) where
  byteSize (Chunk value) = chunkHeaderSize + byteSize value

instance StaticByteSized a => StaticByteSized (Chunk a) where
  staticByteSize _ = chunkHeaderSize + staticByteSize (Proxy :: Proxy a)

instance (StaticLabel a, Binary a) => Binary (Chunk a) where
  get = do
    let !label = staticLabel (Proxy :: Proxy a)
    getExpectLabel label
    chunkSize <- getChunkSize
    -- traceM ("XXX get chunkSize for Chunk " ++ show label ++ ": " ++ show chunkSize)
    value <- getExact chunkSize get
    -- traceM ("XXX value for Chunk" ++ show label ++ ": " ++ show value)
    pure $! Chunk value
  put (Chunk value) = do
    let !label = staticLabel (Proxy :: Proxy a)
    put label
    putChunkSize (byteSize value)
    put value
