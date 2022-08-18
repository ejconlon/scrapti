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

import Control.Monad (unless)
import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, StaticByteSized (..), byteSizeFoldable, getExact,
               getLookAhead, getRemainingSeq, getSkip, putSeq, putWord8)
import Data.Default (Default)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Scrapti.Common (KnownLabel (..), Label, chunkHeaderSize, countSize, getChunkSizeLE, getExpectLabel, labelSize,
                       padCount, putChunkSizeLE)

labelRiff, labelList :: Label
labelRiff = "RIFF"
labelList = "LIST"

listChunkHeaderSize :: ByteCount
listChunkHeaderSize = chunkHeaderSize + labelSize

data Chunk a = Chunk
  { chunkLabel :: !Label
  , chunkBody :: !a
  } deriving stock (Eq, Show)

chunkUnpaddedByteSize :: ByteSized a => Chunk a -> ByteCount
chunkUnpaddedByteSize (Chunk _ body) = byteSize body

instance ByteSized a => ByteSized (Chunk a) where
  byteSize c = padCount (chunkHeaderSize + chunkUnpaddedByteSize c)

instance StaticByteSized a => StaticByteSized (Chunk a) where
  staticByteSize _ = padCount (chunkHeaderSize + staticByteSize (Proxy :: Proxy a))

instance Binary a => Binary (Chunk a) where
  get = do
    lab <- get
    usz <- getChunkSizeLE
    body <- getExact usz get
    unless (even usz) (getSkip 1)
    pure $! Chunk lab body
  put c@(Chunk lab body) = do
    put lab
    let !usz = chunkUnpaddedByteSize c
    putChunkSizeLE usz
    put body
    unless (even usz) (putWord8 0)

newtype KnownChunk a = KnownChunk
  { knownChunkBody :: a
  } deriving stock (Show)
    deriving newtype (Eq, Default)

knownChunkUnpaddedByteSize :: ByteSized a => KnownChunk a -> ByteCount
knownChunkUnpaddedByteSize (KnownChunk body) = byteSize body

instance ByteSized a => ByteSized (KnownChunk a) where
  byteSize c = padCount (chunkHeaderSize + knownChunkUnpaddedByteSize c)

instance StaticByteSized a => StaticByteSized (KnownChunk a) where
  staticByteSize _ = padCount (chunkHeaderSize + staticByteSize (Proxy :: Proxy a))

instance (Binary a, KnownLabel a) => Binary (KnownChunk a) where
  get = do
    getExpectLabel (knownLabel (Proxy :: Proxy a))
    usz <- getChunkSizeLE
    body <- getExact usz get
    unless (even usz) (getSkip 1)
    pure $! KnownChunk body
  put kc@(KnownChunk body) = do
    put (knownLabel (Proxy :: Proxy a))
    let !usz = knownChunkUnpaddedByteSize kc
    putChunkSizeLE usz
    put body
    unless (even usz) (putWord8 0)

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
  byteSize (KnownListChunk body) = padCount (listChunkHeaderSize + byteSizeFoldable body)

instance (Binary a, KnownLabel a) => Binary (KnownListChunk a) where
  get = do
    getExpectLabel labelList
    usz <- getChunkSizeLE
    items <- getExact usz $ do
      getExpectLabel (knownLabel (Proxy :: Proxy a))
      getRemainingSeq get
    unless (even usz) (getSkip 1)
    pure $! KnownListChunk items
  put (KnownListChunk items) = do
    put labelList
    let !usz = labelSize + byteSizeFoldable items
    putChunkSizeLE usz
    put (knownLabel (Proxy :: Proxy a))
    putSeq put items
    unless (even usz) (putWord8 0)

newtype KnownOptChunk a = KnownOptChunk
  { kocItem :: Maybe a
  } deriving stock (Show)
    deriving newtype (Eq, Default)

instance ByteSized a => ByteSized (KnownOptChunk a) where
  byteSize (KnownOptChunk item) = padCount (listChunkHeaderSize + byteSizeFoldable item)

instance (Binary a, KnownLabel a) => Binary (KnownOptChunk a) where
  get = do
    getExpectLabel labelList
    usz <- getChunkSizeLE
    item <- getExact usz $ do
      let !label = knownLabel (Proxy :: Proxy a)
      getExpectLabel label
      if usz == labelSize
        then pure Nothing
        else fmap Just get
    unless (even usz) (getSkip 1)
    pure $! KnownOptChunk item
  put (KnownOptChunk item) = do
    put labelList
    let !usz = labelSize + byteSizeFoldable item
    putChunkSizeLE usz
    put (knownLabel (Proxy :: Proxy a))
    maybe (pure ()) put item
    unless (even usz) (putWord8 0)

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
