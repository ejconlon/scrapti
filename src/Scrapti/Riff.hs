{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Scrapti.Riff
  ( labelRiff
  , labelList
  , listChunkHeaderSize
  , Chunk (..)
  , KnownChunk (..)
  , ListChunkBody (..)
  , ListChunk (..)
  , KnownListChunk (..)
  , KnownOptChunk (..)
  , ChunkLabel (..)
  , peekChunkLabel
  ) where

import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, StaticByteSized (..), byteSizeFoldable, getExact,
               getLookAhead, getRemainingSeq, getSkip, putSeq)
import Data.Default (Default)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Scrapti.Common (KnownLabel (..), Label, chunkHeaderSize, countSize, getChunkSizeLE, getExpectLabel, labelSize,
                       putChunkSizeLE)

labelRiff, labelList :: Label
labelRiff = "RIFF"
labelList = "LIST"

listChunkHeaderSize :: ByteCount
listChunkHeaderSize = chunkHeaderSize + labelSize

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
    sz <- getChunkSizeLE
    body <- getExact sz get
    pure $! Chunk lab body
  put (Chunk lab body) = do
    put lab
    putChunkSizeLE (byteSize body)
    put body

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
    sz <- getChunkSizeLE
    body <- getExact sz get
    pure $! KnownChunk body
  put (KnownChunk body) = do
    put (knownLabel (Proxy :: Proxy a))
    putChunkSizeLE (byteSize body)
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
    sz <- getChunkSizeLE
    items <- getExact sz $ do
      getExpectLabel (knownLabel (Proxy :: Proxy a))
      getRemainingSeq get
    pure $! KnownListChunk items
  put (KnownListChunk items) = do
    put labelList
    putChunkSizeLE (labelSize + byteSizeFoldable items)
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
    sz <- getChunkSizeLE
    item <- getExact sz $ do
      let !label = knownLabel (Proxy :: Proxy a)
      getExpectLabel label
      if sz == labelSize
        then pure Nothing
        else fmap Just get
    pure $! KnownOptChunk item
  put (KnownOptChunk item) = do
    put labelList
    putChunkSizeLE (labelSize + byteSizeFoldable item)
    put (knownLabel (Proxy :: Proxy a))
    maybe (pure ()) put item

data ChunkLabel =
    ChunkLabelSingle !Label
  | ChunkLabelList !Label
  deriving stock (Eq, Show)

peekChunkLabel :: Get ChunkLabel
peekChunkLabel = getLookAhead $ do
  label <- get
  if label == labelList
    then getSkip countSize *> fmap ChunkLabelList get
    else pure (ChunkLabelSingle label)
