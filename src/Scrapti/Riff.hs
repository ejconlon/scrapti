{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Riff
  ( Label
  , labelRiff
  , expectLabel
  , parseExpectLabel
  , getChunkSize
  , parseChunkSize
  , expectChunkSize
  , parseExpectChunkSize
  , putChunkSize
  , chunkHeaderSize
  , StaticLabel (..)
  , Chunk (..)
  ) where

import Data.Default (Default)
import Data.Proxy (Proxy (..))
import Scrapti.Binary (Binary (..), BinaryParser (..), ByteLength, ByteSized (..), FixedBytes, Get, ParseM, Put,
                       StaticByteSized (..), Word32LE, getExpect, getWithoutSize, parseBound, parseExpect)

type Label = FixedBytes 4

labelRiff :: Label
labelRiff = "RIFF"

expectLabel :: Label -> Get ()
expectLabel = getExpect "label" get

parseExpectLabel :: Label -> ParseM ()
parseExpectLabel label = parseExpect "label" label parseWithoutSize

getChunkSize :: Get ByteLength
getChunkSize = fmap fromIntegral (get @Word32LE)

parseChunkSize :: ParseM ByteLength
parseChunkSize = fmap fromIntegral (parseWithoutSize @Word32LE)

expectChunkSize :: ByteLength -> Get ()
expectChunkSize = getExpect "chunk size" getChunkSize

parseExpectChunkSize :: ByteLength -> ParseM ()
parseExpectChunkSize chunkSize = parseExpect "chunkSize" chunkSize parseChunkSize

putChunkSize :: ByteLength -> Put
putChunkSize = put @Word32LE . fromIntegral

chunkHeaderSize :: ByteLength
chunkHeaderSize = 8

class StaticLabel a where
  staticLabel :: Proxy a -> Label

newtype Chunk a = Chunk { chunkValue :: a }
  deriving stock (Show)
  deriving newtype (Eq, Default)

instance (StaticLabel a, BinaryParser a) => Binary (Chunk a) where
  get = getWithoutSize
  put (Chunk value) = do
    let !label = staticLabel (Proxy :: Proxy a)
    put label
    putChunkSize (byteSize value)
    put value

instance ByteSized a => ByteSized (Chunk a) where
  byteSize (Chunk value) = chunkHeaderSize + byteSize value

instance StaticByteSized a => StaticByteSized (Chunk a) where
  staticByteSize = const (chunkHeaderSize + staticByteSize (Proxy :: Proxy a))

instance (StaticLabel a, BinaryParser a) => BinaryParser (Chunk a) where
  parseWithoutSize = do
    let !label = staticLabel (Proxy :: Proxy a)
    parseExpectLabel label
    chunkSize <- parseChunkSize
    value <- parseBound chunkSize parseWithoutSize
    pure $! Chunk value
