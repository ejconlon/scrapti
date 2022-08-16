{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Scrapti.Riff
  ( Label
  , labelSize
  , labelRiff
  , getExpectLabel
  , getChunkSize
  , expectChunkSize
  , putChunkSize
  , chunkHeaderSize
  , ChunkPair (..)
  , KnownLabel (..)
  , KnownChunkPair (..)
  ) where

import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, Put, StaticByteSized (..), StaticBytes, Word32LE, getExact,
               getExpect)
import Data.Default (Default)
import Data.Proxy (Proxy (..))
import Scrapti.Binary (DepBinary (..))

type Label = StaticBytes 4

labelSize :: ByteCount
labelSize = 4

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

-- data ChunkHeader = ChunkHeader
--   { chLabel :: !Label
--   , chSize :: !ByteCount
--   } deriving stock (Eq, Show)

-- instance ByteSized ChunkHeader where
--   byteSize _ = chunkHeaderSize

-- instance StaticByteSized ChunkHeader where
--   staticByteSize _ = chunkHeaderSize

-- instance Binary ChunkHeader where
--   get = do
--     lab <- get
--     sz <- getChunkSize
--     pure $! ChunkHeader lab sz
--   put (ChunkHeader lab sz) = do
--     put lab
--     putChunkSize sz

data ChunkPair a = ChunkPair
  { cpLabel :: !Label
  , cpBody :: !a
  } deriving stock (Eq, Show)

instance ByteSized a => ByteSized (ChunkPair a) where
  byteSize (ChunkPair _ body) = chunkHeaderSize + byteSize body

instance StaticByteSized a => StaticByteSized (ChunkPair a) where
  staticByteSize _ = chunkHeaderSize + staticByteSize (Proxy :: Proxy a)

-- cpHeader :: ByteSized a => ChunkPair a -> ChunkHeader
-- cpHeader (ChunkPair lab body) = ChunkHeader lab (byteSize body)

instance DepBinary Label a => Binary (ChunkPair a) where
  get = do
    lab <- get
    sz <- getChunkSize
    body <- getExact sz (getDep lab)
    pure $! ChunkPair lab body
  put (ChunkPair lab body) = do
    put lab
    putChunkSize (byteSize body)
    putDep lab body

class KnownLabel a where
  knownLabel :: Proxy a -> Label

newtype KnownChunkPair a = KnownChunkPair
  { kcpBody :: a
  } deriving stock (Show)
    deriving newtype (Eq, Default)

instance ByteSized a => ByteSized (KnownChunkPair a) where
  byteSize (KnownChunkPair body) = chunkHeaderSize + byteSize body

instance StaticByteSized a => StaticByteSized (KnownChunkPair a) where
  staticByteSize _ = chunkHeaderSize + staticByteSize (Proxy :: Proxy a)

instance (Binary a, KnownLabel a) => Binary (KnownChunkPair a) where
  get = do
    getExpectLabel (knownLabel (Proxy :: Proxy a))
    sz <- getChunkSize
    body <- getExact sz get
    pure $! KnownChunkPair body
  put (KnownChunkPair body) = do
    put (knownLabel (Proxy :: Proxy a))
    putChunkSize (byteSize body)
    put body
