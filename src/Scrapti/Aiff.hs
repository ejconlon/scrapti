{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scrapti.Aiff
  ( AiffChunk (..)
  , Aiff (..)
  , lookupAiffChunk
  , lookupAiffCommonChunk
  , lookupAiffDataChunk
  , aiffGetPcmContainer
  , aiffSetPcmContainer
  , aiffPcmContainerLens
  ) where

import Control.Monad (unless)
import Dahdit (Binary (..), ByteArray, ByteCount (..), ByteSized (..), ShortByteString, StaticByteSized (..),
               StaticBytes, ViaGeneric (..), ViaStaticByteSized (..), Word16BE, Word32BE, byteSizeFoldable,
               getByteString, getExact, getLookAhead, getRemainingByteArray, getRemainingSeq, getSkip, getWord8,
               putByteArray, putByteString, putSeq, putWord8)
import qualified Data.ByteString.Short as BSS
import Data.Default (Default (..))
import Data.Primitive.ByteArray (sizeofByteArray)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Lens.Micro (lens)
import Scrapti.Common (KnownLabel (..), Label, UnparsedBody, chunkHeaderSize, countSize, getChunkSizeBE, getExpectLabel,
                       labelSize, padCount, putChunkSizeBE)
import Scrapti.Dsp (PcmContainer, PcmContainerLens)

-- AIFF-C file parsing according to
-- http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/AIFF/AIFF.html
-- We only bother to support "sowt" compression (little endian samples)

-- AIFF shares a similar 4-byte label + size + payload structure with RIFF
-- We could use a lot of the same structures to read the file... If they were
-- big-endian.

labelForm, labelAifc, labelComm, labelSsnd, labelFver, labelAnno, labelMark :: Label
labelForm = "FORM"
labelAifc = "AIFC"
labelComm = "COMM"
labelSsnd = "SSND"
labelFver = "FVER"
labelAnno = "ANNO"
labelMark = "MARK"

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
    usz <- getChunkSizeBE
    body <- getExact usz get
    unless (even usz) (getSkip 1)
    pure $! Chunk lab body
  put c@(Chunk lab body) = do
    put lab
    let !usz = chunkUnpaddedByteSize c
    putChunkSizeBE usz
    put body
    unless (even usz) (putWord8 0)

newtype KnownChunk a = KnownChunk
  { knownChunkBody :: a
  } deriving stock (Show)
    deriving newtype (Eq, Default)

knownChunkUnpaddedByteSize :: ByteSized a => KnownChunk a -> ByteCount
knownChunkUnpaddedByteSize (KnownChunk body) = byteSize body

instance ByteSized a => ByteSized (KnownChunk a) where
  byteSize kc = padCount (chunkHeaderSize + knownChunkUnpaddedByteSize kc)

instance StaticByteSized a => StaticByteSized (KnownChunk a) where
  staticByteSize _ = padCount (chunkHeaderSize + staticByteSize (Proxy :: Proxy a))

instance (Binary a, KnownLabel a) => Binary (KnownChunk a) where
  get = do
    getExpectLabel (knownLabel (Proxy :: Proxy a))
    usz <- getChunkSizeBE
    body <- getExact usz get
    unless (even usz) (getSkip 1)
    pure $! KnownChunk body
  put kc@(KnownChunk body) = do
    put (knownLabel (Proxy :: Proxy a))
    let !usz = knownChunkUnpaddedByteSize kc
    putChunkSizeBE usz
    put body
    unless (even usz) (putWord8 0)

-- | A "Pascal-style string" with a leading byte count and optional
-- trailing padding byte to make total length even.
newtype PascalString = PascalString { unPString :: ShortByteString }
  deriving stock (Show)
  deriving newtype (Eq)

instance Default PascalString where
  def = PascalString BSS.empty

instance ByteSized PascalString where
  byteSize (PascalString sbs) = padCount (byteSize sbs + 1)

instance Binary PascalString where
  get = do
    usz <- fmap fromIntegral getWord8
    sbs <- getByteString usz
    unless (odd usz) (getSkip 1)
    pure $! PascalString sbs
  put (PascalString sbs) = do
    let !usz = fromIntegral (BSS.length sbs)
    putWord8 usz
    putByteString sbs
    unless (odd usz) (putWord8 0)

-- | "80 bit IEEE Standard 754 floating point number"
newtype ExtendedFloat = ExtendedFloat { unExtendedFloat :: StaticBytes 10 }
  deriving stock (Show)
  deriving newtype (Eq, Default, ByteSized, StaticByteSized, Binary)

data AiffCommonBody = AiffCommonBody
  { aceNumChannels :: !Word16BE
  , aceNumSampleFrames :: !Word32BE
  , aceSampleSize :: !Word16BE
  , aceSampleRate :: !ExtendedFloat
  , aceCompressionType :: !Label
  , aceCompressionName :: !PascalString
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, Binary) via (ViaGeneric AiffCommonBody)

instance KnownLabel AiffCommonBody where
  knownLabel _ = labelComm

type AiffCommonChunk = KnownChunk AiffCommonBody

data AiffDataBody = AiffDataBody
  { adbOffset :: !Word32BE
  , adbBlockSize :: !Word32BE
  , adbSoundData :: !ByteArray
  } deriving stock (Eq, Show, Generic)

instance ByteSized AiffDataBody where
  byteSize (AiffDataBody _ _ arr) = 8 + fromIntegral (sizeofByteArray arr)

instance Binary AiffDataBody where
  get = do
    adbOffset <- get
    adbBlockSize <- get
    adbSoundData <- getRemainingByteArray
    pure $! AiffDataBody {..}
  put (AiffDataBody {..}) = do
    put adbOffset
    put adbBlockSize
    putByteArray adbSoundData

instance KnownLabel AiffDataBody where
  knownLabel _ = labelSsnd

type AiffDataChunk = KnownChunk AiffDataBody

type AiffVersionChunk = Chunk UnparsedBody
type AiffAnnoChunk = Chunk UnparsedBody
type AiffMarkChunk = Chunk UnparsedBody
type AiffUnparsedChunk = Chunk UnparsedBody

data AiffChunk =
    AiffChunkCommon !AiffCommonChunk
  | AiffChunkData !AiffDataChunk
  | AiffChunkVersion !AiffVersionChunk
  | AiffChunkAnno !AiffAnnoChunk
  | AiffChunkMark !AiffMarkChunk
  | AiffChunkUnparsed !AiffUnparsedChunk
  deriving stock (Eq, Show, Generic)

instance ByteSized AiffChunk where
  byteSize = \case
    AiffChunkCommon x -> byteSize x
    AiffChunkData x -> byteSize x
    AiffChunkVersion x -> byteSize x
    AiffChunkAnno x -> byteSize x
    AiffChunkMark x -> byteSize x
    AiffChunkUnparsed x -> byteSize x

instance Binary AiffChunk where
  get = do
    label <- getLookAhead get
    if
      | label == labelComm -> fmap AiffChunkCommon get
      | label == labelSsnd -> fmap AiffChunkData get
      | label == labelFver -> fmap AiffChunkVersion get
      | label == labelAnno -> fmap AiffChunkAnno get
      | label == labelMark -> fmap AiffChunkMark get
      | otherwise -> fmap AiffChunkUnparsed get
  put = \case
    AiffChunkCommon x -> put x
    AiffChunkData x -> put x
    AiffChunkVersion x -> put x
    AiffChunkAnno x -> put x
    AiffChunkMark x -> put x
    AiffChunkUnparsed x -> put x

newtype AiffHeader = AiffHeader
  { wavHeaderRemainingSize :: ByteCount
  } deriving stock (Show, Generic)
    deriving newtype (Eq)
    deriving (ByteSized) via (ViaStaticByteSized AiffHeader)

aiffHeaderSize :: ByteCount
aiffHeaderSize = 2 * labelSize + countSize

instance StaticByteSized AiffHeader where
  staticByteSize _ = aiffHeaderSize

instance Binary AiffHeader where
  get = do
    getExpectLabel labelForm
    sz <- getChunkSizeBE
    getExpectLabel labelAifc
    pure $! AiffHeader (sz - labelSize)
  put (AiffHeader remSz) = do
    put labelForm
    putChunkSizeBE (remSz + labelSize)
    put labelAifc

newtype Aiff = Aiff
  { aiffChunks :: Seq AiffChunk
  } deriving stock (Show)
    deriving newtype (Eq)

instance ByteSized Aiff where
  byteSize (Aiff chunks) = aiffHeaderSize + byteSizeFoldable chunks

instance Binary Aiff where
  get = do
    AiffHeader remSz <- get
    chunks <- getExact remSz (getRemainingSeq get)
    pure $! Aiff chunks
  put (Aiff chunks)= do
    let !remSz = byteSizeFoldable chunks
    put (AiffHeader remSz)
    putSeq put chunks

lookupAiffChunk :: (AiffChunk -> Bool) -> Aiff -> Maybe AiffChunk
lookupAiffChunk p (Aiff chunks) = fmap (Seq.index chunks) (Seq.findIndexL p chunks)

lookupAiffCommonChunk :: Aiff -> Maybe AiffCommonChunk
lookupAiffCommonChunk w =
  case lookupAiffChunk (\case { AiffChunkCommon _ -> True; _ -> False }) w of
    Just (AiffChunkCommon x) -> Just x
    _ -> Nothing

lookupAiffDataChunk :: Aiff -> Maybe AiffDataChunk
lookupAiffDataChunk w =
  case lookupAiffChunk (\case { AiffChunkData _ -> True; _ -> False }) w of
    Just (AiffChunkData x) -> Just x
    _ -> Nothing

aiffGetPcmContainer :: Aiff -> PcmContainer
aiffGetPcmContainer = error "TODO"

aiffSetPcmContainer :: Aiff -> PcmContainer -> Aiff
aiffSetPcmContainer = error "TODO"

aiffPcmContainerLens :: PcmContainerLens Aiff
aiffPcmContainerLens = lens aiffGetPcmContainer aiffSetPcmContainer
