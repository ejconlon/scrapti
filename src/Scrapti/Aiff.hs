{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scrapti.Aiff where

import Control.Monad (unless)
import Dahdit (Binary (..), ByteCount (..), ByteSized (..), LiftedPrim, LiftedPrimArray, Seq, ShortByteString,
               StaticByteSized (..), StaticBytes, ViaGeneric (..), ViaStaticByteSized (..), Word16BE, Word32BE,
               byteSizeFoldable, getByteString, getExact, getRemainingLiftedPrimArray, getRemainingSeq, getSkip,
               getWord8, putByteString, putLiftedPrimArray, putSeq, putWord8)
import qualified Data.ByteString.Short as BSS
import Data.Default (Default (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Scrapti.Common (KnownLabel (..), Label, UnparsedBody, chunkHeaderSize, countSize, getChunkSizeBE, getExpectLabel,
                       labelSize, padCount, putChunkSizeBE)

-- AIFF-C file parsing according to
-- http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/AIFF/AIFF.html
-- We only bother to support "sowt" compression (little endian samples)

-- AIFF shares a similar 4-byte label + size + payload structure with RIFF
-- We could use a lot of the same structures to read the file... If they were
-- big-endian.

labelForm, labelAifc, labelComm, labelSsnd :: Label
labelForm = "FORM"
labelAifc = "AIFC"
labelComm = "COMM"
labelSsnd = "SSND"

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
knownChunkUnpaddedByteSize (KnownChunk body) = chunkHeaderSize + byteSize body

instance ByteSized a => ByteSized (KnownChunk a) where
  byteSize = padCount . knownChunkUnpaddedByteSize

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

-- TODO does the size include the padding byte?
-- We assume it doesn't here - change to be like PaddedString if so
instance Binary PascalString where
  get = do
    usz <- fmap fromIntegral getWord8
    sbs <- getByteString usz
    unless (even usz) (getSkip 1)
    pure $! PascalString sbs
  put (PascalString sbs) = do
    let !usz = fromIntegral (BSS.length sbs)
    putWord8 usz
    putByteString sbs
    unless (even usz) (putWord8 0)

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

data AiffDataBody a = AiffDataBody
  { adbOffset :: !Word32BE
  , adbBlockSize :: !Word32BE
  , adbSoundData :: !(LiftedPrimArray a)
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized) via (ViaGeneric (AiffDataBody a))

instance (StaticByteSized a, LiftedPrim a) => Binary (AiffDataBody a) where
  get = do
    adbOffset <- get
    adbBlockSize <- get
    adbSoundData <- getRemainingLiftedPrimArray (Proxy :: Proxy a)
    pure $! AiffDataBody {..}
  put (AiffDataBody {..}) = do
    put adbOffset
    put adbBlockSize
    putLiftedPrimArray adbSoundData

instance KnownLabel (AiffDataBody a) where
  knownLabel _ = labelSsnd

type AiffDataChunk a = KnownChunk (AiffDataBody a)

data AiffBody a = AiffBody
  deriving stock (Eq, Show)

type AiffExtraChunk = Chunk UnparsedBody

data Aiff a = Aiff
  { aiffCommon :: !AiffCommonChunk
  , aiffData :: !(AiffDataChunk a)
  , aiffExtras :: !(Seq AiffExtraChunk)
  } deriving stock (Eq, Show, Generic)

instance (StaticByteSized a, LiftedPrim a) => ByteSized (Aiff a) where
  byteSize (Aiff {..}) = byteSize aiffCommon + byteSize aiffData + byteSizeFoldable aiffExtras

newtype AiffHeader = AiffHeader
  { ahSize :: ByteCount
  } deriving stock (Show, Generic)
    deriving newtype (Eq)
    deriving (ByteSized) via (ViaStaticByteSized AiffHeader)

instance StaticByteSized AiffHeader where
  staticByteSize _ = 2 * labelSize + countSize

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

instance (StaticByteSized a, LiftedPrim a) => Binary (Aiff a) where
  get = do
    AiffHeader remSz <- get
    getExact remSz $ do
      aiffCommon <- get
      let width = fromIntegral (aceSampleSize (knownChunkBody aiffCommon))
          staticWidth = staticByteSize (Proxy :: Proxy a)
      unless (width == staticWidth) (fail ("Bad sample width, expected " ++ show (unByteCount staticWidth) ++ " but read " ++ show (unByteCount width)))
      aiffData <- get
      aiffExtras <- getRemainingSeq get
      pure $! Aiff {..}
  put aiff@(Aiff {..})= do
    let !remSz = byteSize aiff
    put (AiffHeader remSz)
    put aiffCommon
    put aiffData
    putSeq put aiffExtras
