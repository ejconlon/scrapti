{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Scrapti.Riff
  ( Label
  , labelSize
  , labelRiff
  , labelList
  , getExpectLabel
  , getChunkSize
  , expectChunkSize
  , putChunkSize
  , chunkHeaderSize
  , listChunkHeaderSize
  , Chunk (..)
  , KnownLabel (..)
  , KnownChunk (..)
  , ListChunkBody (..)
  , ListChunk (..)
  , KnownListChunk (..)
  , KnownOptChunk (..)
  ) where

import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, Put, StaticByteSized (..), StaticBytes, Word32LE,
               byteSizeFoldable, getExact, getExpect, getRemainingSeq, putSeq)
import Data.Default (Default)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)

type Label = StaticBytes 4

labelSize :: ByteCount
labelSize = 4

labelRiff, labelList :: Label
labelRiff = "RIFF"
labelList = "LIST"

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

listChunkHeaderSize :: ByteCount
listChunkHeaderSize = chunkHeaderSize + labelSize

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

data Chunk a = Chunk
  { chunkLabel :: !Label
  , chunkBody :: !a
  } deriving stock (Eq, Show)

instance ByteSized a => ByteSized (Chunk a) where
  byteSize (Chunk _ body) = chunkHeaderSize + byteSize body

instance StaticByteSized a => StaticByteSized (Chunk a) where
  staticByteSize _ = chunkHeaderSize + staticByteSize (Proxy :: Proxy a)

instance Binary a => Binary (Chunk a) where
  get = do
    lab <- get
    sz <- getChunkSize
    body <- getExact sz get
    pure $! Chunk lab body
  put (Chunk lab body) = do
    put lab
    putChunkSize (byteSize body)
    put body

class KnownLabel a where
  knownLabel :: Proxy a -> Label

newtype KnownChunk a = KnownChunk
  { knownChunkBody :: a
  } deriving stock (Show)
    deriving newtype (Eq, Default)

instance ByteSized a => ByteSized (KnownChunk a) where
  byteSize (KnownChunk body) = chunkHeaderSize + byteSize body

instance StaticByteSized a => StaticByteSized (KnownChunk a) where
  staticByteSize _ = chunkHeaderSize + staticByteSize (Proxy :: Proxy a)

instance (Binary a, KnownLabel a) => Binary (KnownChunk a) where
  get = do
    getExpectLabel (knownLabel (Proxy :: Proxy a))
    sz <- getChunkSize
    body <- getExact sz get
    pure $! KnownChunk body
  put (KnownChunk body) = do
    put (knownLabel (Proxy :: Proxy a))
    putChunkSize (byteSize body)
    put body

data ListChunkBody a = ListChunkBody
  { lcbLabel :: !Label
  , lcbItems :: !(Seq a)
  } deriving stock (Eq, Show)

instance ByteSized a => ByteSized (ListChunkBody a) where
  byteSize (ListChunkBody _ items) = labelSize + byteSizeFoldable items

instance Binary a => Binary (ListChunkBody a) where
  get = do
    label <- get
    items <- getRemainingSeq get
    pure $! ListChunkBody label items
  put (ListChunkBody label items) = do
    put label
    putSeq put items

instance KnownLabel (ListChunkBody a) where
  knownLabel _ = labelList

newtype ListChunk a = ListChunk { unListChunk :: KnownChunk (ListChunkBody a) }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, Binary)

newtype KnownListChunk a = KnownListChunk
  { klcItems :: Seq a
  } deriving stock (Show)
    deriving newtype (Eq, Default)

instance ByteSized a => ByteSized (KnownListChunk a) where
  byteSize (KnownListChunk body) = listChunkHeaderSize + byteSizeFoldable body

instance (Binary a, KnownLabel a) => Binary (KnownListChunk a) where
  get = do
    getExpectLabel labelList
    sz <- getChunkSize
    items <- getExact sz $ do
      getExpectLabel (knownLabel (Proxy :: Proxy a))
      getRemainingSeq get
    pure $! KnownListChunk items
  put (KnownListChunk items) = do
    put labelList
    putChunkSize (labelSize + byteSizeFoldable items)
    put (knownLabel (Proxy :: Proxy a))
    putSeq put items

newtype KnownOptChunk a = KnownOptChunk
  { kocItem :: Maybe a
  } deriving stock (Show)
    deriving newtype (Eq, Default)

instance ByteSized a => ByteSized (KnownOptChunk a) where
  byteSize (KnownOptChunk item) = listChunkHeaderSize + byteSizeFoldable item

instance (Binary a, KnownLabel a) => Binary (KnownOptChunk a) where
  get = do
    getExpectLabel labelList
    sz <- getChunkSize
    item <- getExact sz $ do
      let !label = knownLabel (Proxy :: Proxy a)
      getExpectLabel label
      if sz == labelSize
        then pure Nothing
        else fmap Just get
    pure $! KnownOptChunk item
  put (KnownOptChunk item) = do
    put labelList
    putChunkSize (labelSize + byteSizeFoldable item)
    put (knownLabel (Proxy :: Proxy a))
    maybe (pure ()) put item
