{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Wav
  ( Sampled (..)
  , WavFormatBody (..)
  , WavFormatChunk (..)
  , WavHeader (..)
  , WavDataBody (..)
  , WavUnparsedBody (..)
  , WavExtraBody (..)
  , WavChoice (..)
  , WavBody (..)
  , Wav (..)
  , SampledWav (..)
  , labelWave
  ) where

import Control.Monad (unless)
import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, Int16LE, Int32LE, PrimArray, ShortByteString,
               StaticByteSized (..), Word16LE, Word32LE, byteSizeFoldable, getExact, getRemainingSeq,
               getRemainingStaticArray, getRemainingString, getUnfold, putByteString, putSeq, putStaticArray)
import Data.Default (Default (..))
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import Scrapti.Binary (DepBinary (..))
import Scrapti.Riff (Chunk (..), KnownChunk (..), KnownLabel (..), Label, chunkHeaderSize, getChunkSize, getExpectLabel,
                     labelRiff, labelSize, putChunkSize)

labelWave, labelFmt, labelData :: Label
labelWave = "WAVE"
labelFmt = "fmt "
labelData = "data"

newtype BitLength = BitLength { unBitLength :: Word16LE }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default, Binary, ByteSized)

data Sampled f where
  Sampled :: (Prim a, Binary a, StaticByteSized a) => !(f a) -> Sampled f

getSampled :: BitLength -> Maybe (Sampled Proxy)
getSampled = \case
  8 -> Just (Sampled (Proxy :: Proxy Int8))
  16 -> Just (Sampled (Proxy :: Proxy Int16LE))
  -- 24 -> Just (Sampled (Proxy :: Proxy Int24LE))
  32 -> Just (Sampled (Proxy :: Proxy Int32LE))
  -- 64 -> Just (Sampled (Proxy :: Proxy Int64LE))
  _ -> Nothing

data WavFormatBody = WavFormatBody
  { wfbFormatType :: !Word16LE
  , wfbNumChannels :: !Word16LE
  , wfbSampleRate :: !Word32LE
  , wfbBitsPerSample :: !BitLength
  , wfbExtra :: !ShortByteString
  } deriving stock (Eq, Show)

instance Default WavFormatBody where
  def = WavFormatBody 1 2 44100 16 mempty

instance ByteSized WavFormatBody where
  byteSize wf = 16 + byteSize (wfbExtra wf)

isSupportedBPS :: BitLength -> Bool
isSupportedBPS w = mod w 8 == 0 && w <= 64

isSupportedFmtExtraSize :: ByteCount -> Bool
isSupportedFmtExtraSize x = x == 0 || x == 2 || x == 24

instance Binary WavFormatBody where
  get = do
    formatType <- get
    numChannels <- get
    sampleRate <- get
    bpsAvg <- get
    bpsSlice <- get
    bps <- get
    unless (isSupportedBPS bps) (fail ("Bad bps: " ++ show bps))
    unless (bpsSlice == div (fromIntegral bps) 8 * numChannels) (fail ("Bad bps slice: " ++ show bpsSlice))
    unless (bpsAvg == sampleRate * fromIntegral bpsSlice) (fail ("Bad average bps: " ++ show bpsAvg))
    extra <- getRemainingString
    let !extraLen = byteSize extra
    unless (isSupportedFmtExtraSize extraLen) (fail ("Bad extra length: " ++ show extraLen))
    pure $! WavFormatBody formatType numChannels sampleRate bps extra
  put (WavFormatBody fty nchan sr bps extra) = do
    let !bpsSlice = div (fromIntegral bps) 8 * nchan
    let !bpsAvg = sr * fromIntegral bpsSlice
    put fty
    put nchan
    put sr
    put bpsAvg
    put bpsSlice
    put bps
    putByteString extra

instance KnownLabel WavFormatBody where
  knownLabel _ = labelFmt

newtype WavFormatChunk = WavFormatChunk { unWavFormatChunk :: KnownChunk WavFormatBody }
  deriving stock (Show)
  deriving newtype (Eq, Binary, ByteSized, Default)

newtype WavDataBody a = WavDataBody { unWavDataBody :: PrimArray a }
  deriving stock (Show)
  deriving newtype (Eq)

instance KnownLabel (WavDataBody a) where
  knownLabel _ = labelData

instance (Prim a, StaticByteSized a) => ByteSized (WavDataBody a) where
  byteSize (WavDataBody vec) = byteSize vec

instance (Prim a, StaticByteSized a) => Binary (WavDataBody a) where
  get = do
    arr <- getRemainingStaticArray (Proxy :: Proxy a)
    pure $! WavDataBody arr
  put (WavDataBody arr) = do
    putStaticArray arr

instance Default (WavDataBody a) where
  def = WavDataBody mempty

data WavHeader = WavHeader
  { wavHeaderRemainingSize :: !ByteCount
  , wavHeaderFormat :: !WavFormatChunk
  } deriving stock (Eq, Show)

instance ByteSized WavHeader where
  byteSize (WavHeader _ format) = chunkHeaderSize + labelSize + byteSize format

instance Binary WavHeader where
  get = do
    getExpectLabel labelRiff
    fileSize <- getChunkSize
    getExpectLabel labelWave
    format <- get
    let !formatSize = byteSize format
    let !remainingSize = fileSize - formatSize - labelSize
    pure $! WavHeader remainingSize format
  put (WavHeader remainingSize format) = do
    let !formatSize = byteSize format
        !fileSize = remainingSize + formatSize + labelSize
    put labelRiff
    putChunkSize fileSize
    put labelWave
    put format

newtype WavUnparsedBody = WavUnparsedBody
  { wubContents :: ShortByteString
  } deriving stock (Show)
    deriving newtype (Eq)

instance ByteSized WavUnparsedBody where
  byteSize (WavUnparsedBody bs) = byteSize bs

instance Binary WavUnparsedBody where
  get = fmap WavUnparsedBody getRemainingString
  put (WavUnparsedBody bs) = putByteString bs

newtype WavExtraBody =
    WavExtraBodyUnparsed WavUnparsedBody
  -- | WavExtraBodyMetadata
  deriving stock (Eq, Show)

instance ByteSized WavExtraBody where
  byteSize = \case
    WavExtraBodyUnparsed wub -> byteSize wub

instance DepBinary Label WavExtraBody where
  getDep _ = fmap WavExtraBodyUnparsed get
  putDep _ = \case
    WavExtraBodyUnparsed wub -> put wub

data WavChoice a =
    WavChoiceExtra !WavExtraBody
  | WavChoiceData !(WavDataBody a)
  deriving stock (Eq, Show)

instance (Prim a, StaticByteSized a) => ByteSized (WavChoice a) where
  byteSize = \case
    WavChoiceExtra wce -> byteSize wce
    WavChoiceData wcd -> byteSize wcd

instance (Prim a, StaticByteSized a) => DepBinary Label (WavChoice a) where
  getDep label = do
    if label == labelData
      then fmap WavChoiceData get
      else fmap WavChoiceExtra (getDep label)
  putDep label = \case
    WavChoiceExtra wce -> putDep label wce
    WavChoiceData wcd -> put wcd

data WavBody a = WavBody
  { wbUnparsedPre :: !(Seq (Chunk WavExtraBody))
  , wbSample :: !(KnownChunk (WavDataBody a))
  , wbUnparsedPost :: !(Seq (Chunk WavExtraBody))
  } deriving stock (Eq, Show)

instance Default (WavBody a) where
  def = WavBody Empty def Empty

instance (Prim a, StaticByteSized a) => ByteSized (WavBody a) where
  byteSize (WavBody pre sam post) = byteSizeFoldable pre + byteSize sam + byteSizeFoldable post

instance (Prim a, StaticByteSized a) => Binary (WavBody a) where
  get = do
    (!pre, !datBody) <- getUnfold Empty $ \pre -> do
      Chunk lab body <- get
      pure $! case body of
        WavChoiceExtra wce -> Left (pre :|> Chunk lab wce)
        WavChoiceData wcd -> Right (pre, wcd)
    post <- getRemainingSeq get
    pure $! WavBody pre (KnownChunk datBody) post
  put (WavBody pre dat post) = do
    putSeq put pre
    put dat
    putSeq put post

data Wav a = Wav
  { wavFormat :: !WavFormatChunk
  , wavBody :: !(WavBody a)
  } deriving stock (Eq, Show)

instance Default (Wav a) where
  def = Wav def def

instance (Prim a, StaticByteSized a) => ByteSized (Wav a) where
  byteSize (Wav fmt body) =
    let !remainingSize = byteSize body
        !header = WavHeader remainingSize fmt
    in byteSize header + byteSize body

getRestOfWav :: (Prim a, StaticByteSized a) => Proxy a -> ByteCount -> WavFormatChunk -> Get (Wav a)
getRestOfWav _ remainingSize fmtChunk = do
  body <- getExact remainingSize get
  pure $! Wav fmtChunk body

instance (Prim a, StaticByteSized a) => Binary (Wav a) where
  get = do
    WavHeader remainingSize fmtChunk <- get
    let !fmt = knownChunkBody (unWavFormatChunk fmtChunk)
        !fmtBps = fromIntegral (wfbBitsPerSample fmt)
        !prox = Proxy :: Proxy a
        !parseBps = staticByteSize prox * 8
    unless (fmtBps == parseBps) (fail ("Bad bps: in header: " ++ show fmtBps ++ " required: " ++ show parseBps))
    getRestOfWav prox remainingSize fmtChunk
  put (Wav fmtChunk body) = do
    let !remainingSize = byteSize body
        !header = WavHeader remainingSize fmtChunk
    put header
    put body

newtype SampledWav = SampledWav { unSampledWav :: Sampled Wav }

instance ByteSized SampledWav where
  byteSize (SampledWav (Sampled wd)) = byteSize wd

instance Binary SampledWav where
  get = do
    WavHeader remainingSize fmtChunk <- get
    let !fmt = knownChunkBody (unWavFormatChunk fmtChunk)
        !bps = wfbBitsPerSample fmt
    case getSampled bps of
      Nothing -> fail ("Bad bps: " ++ show bps)
      Just (Sampled prox) -> fmap (SampledWav . Sampled) (getRestOfWav prox remainingSize fmtChunk)
  put (SampledWav (Sampled wd)) = put wd
