{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Wav
  ( Sampled (..)
  , WavFormat (..)
  , WavFormatChunk (..)
  , WavHeader (..)
  , WavSampleChunk (..)
  , WavUnparsedChunk (..)
  , WavChunk (..)
  , WavBody (..)
  , Wav (..)
  , SampledWav (..)
  , labelWave
  ) where

import Control.Monad (unless)
import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, Int16LE, Int32LE, PrimArray, ShortByteString,
               StaticByteSized (..), Word16LE, Word32LE, byteSizeFoldable, getByteString, getExact, getRemainingSeq,
               getRemainingStaticArray, getRemainingString, getUnfold, putByteString, putSeq, putStaticArray)
import Data.Default (Default (..))
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import Scrapti.Riff (Chunk (..), Label, StaticLabel (..), chunkHeaderSize, getChunkSize, getExpectLabel, labelRiff,
                     labelSize, putChunkSize)

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

data WavFormat = WavFormat
  { wfFormatType :: !Word16LE
  , wfNumChannels :: !Word16LE
  , wfSampleRate :: !Word32LE
  , wfBitsPerSample :: !BitLength
  , wfExtra :: !ShortByteString
  } deriving stock (Eq, Show)

instance Default WavFormat where
  def = WavFormat 1 2 44100 16 mempty

instance ByteSized WavFormat where
  byteSize wf = 16 + byteSize (wfExtra wf)

isSupportedBPS :: BitLength -> Bool
isSupportedBPS w = mod w 8 == 0 && w <= 64

isSupportedFmtExtraSize :: ByteCount -> Bool
isSupportedFmtExtraSize x = x == 0 || x == 2 || x == 24

instance Binary WavFormat where
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
    pure $! WavFormat formatType numChannels sampleRate bps extra
  put (WavFormat fty nchan sr bps extra) = do
    let !bpsSlice = div (fromIntegral bps) 8 * nchan
    let !bpsAvg = sr * fromIntegral bpsSlice
    put fty
    put nchan
    put sr
    put bpsAvg
    put bpsSlice
    put bps
    putByteString extra

instance StaticLabel WavFormat where
  staticLabel _ = labelFmt

newtype WavFormatChunk = WavFormatChunk { unWavFormatChunk :: Chunk WavFormat }
  deriving stock (Show)
  deriving newtype (Eq, Binary, ByteSized, Default)

newtype WavSampleChunk a = WavSampleChunk { unWavSampleChunk :: PrimArray a }
  deriving stock (Show)
  deriving newtype (Eq)

instance (Prim a, StaticByteSized a) => ByteSized (WavSampleChunk a) where
  byteSize (WavSampleChunk vec) = 8 + byteSize vec

instance (Prim a, StaticByteSized a) => Binary (WavSampleChunk a) where
  get = do
    getExpectLabel labelData
    getSampleChunkPostLabel
  put (WavSampleChunk arr) = do
    put labelData
    let !chunkSize = byteSize arr
    putChunkSize chunkSize
    putStaticArray arr

getSampleChunkPostLabel :: (Prim a, StaticByteSized a) => Get (WavSampleChunk a)
getSampleChunkPostLabel = do
  chunkSize <- getChunkSize
  arr <- getExact chunkSize (getRemainingStaticArray (Proxy :: Proxy a))
  pure $! WavSampleChunk arr

instance Default (WavSampleChunk a) where
  def = WavSampleChunk mempty

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

data WavUnparsedChunk = WavUnparsedChunk
  { wavUnparsedLabel :: !Label
  , wavUnparsedContents :: !ShortByteString
  } deriving stock (Eq, Show)

instance ByteSized WavUnparsedChunk where
  byteSize (WavUnparsedChunk _ bs) = chunkHeaderSize + byteSize bs

getUnparsedChunkPostLabel :: Label -> Get WavUnparsedChunk
getUnparsedChunkPostLabel label = do
  chunkSize <- getChunkSize
  bs <- getByteString chunkSize
  pure $! WavUnparsedChunk label bs

instance Binary WavUnparsedChunk where
  get = do
    label <- get
    getUnparsedChunkPostLabel label
  put (WavUnparsedChunk label bs) = do
    put label
    putChunkSize (byteSize bs)
    putByteString bs

data WavChunk a =
    WavChunkUnparsed !WavUnparsedChunk
  | WavChunkSample !(WavSampleChunk a)
  deriving stock (Eq, Show)

instance (Prim a, StaticByteSized a) => ByteSized (WavChunk a) where
  byteSize = \case
    WavChunkUnparsed wuc -> byteSize wuc
    WavChunkSample wsc -> byteSize wsc

instance (Prim a, StaticByteSized a) => Binary (WavChunk a) where
  get = do
    label <- get
    if label == labelData
      then fmap WavChunkSample getSampleChunkPostLabel
      else fmap WavChunkUnparsed (getUnparsedChunkPostLabel label)
  put = \case
    WavChunkUnparsed wuc -> put wuc
    WavChunkSample wsc -> put wsc

data WavBody a = WavBody
  { wbUnparsedPre :: !(Seq WavUnparsedChunk)
  , wbSample :: !(WavSampleChunk a)
  , wbUnparsedPost :: !(Seq WavUnparsedChunk)
  } deriving stock (Eq, Show)

instance Default (WavBody a) where
  def = WavBody Empty def Empty

instance (Prim a, StaticByteSized a) => ByteSized (WavBody a) where
  byteSize (WavBody pre sam post) = byteSizeFoldable pre + byteSize sam + byteSizeFoldable post

instance (Prim a, StaticByteSized a) => Binary (WavBody a) where
  get = do
    (!pre, !sam) <- getUnfold Empty $ \pre -> do
      chunk <- get
      pure $! case chunk of
        WavChunkUnparsed wuc -> Left (pre :|> wuc)
        WavChunkSample wsc -> Right (pre, wsc)
    post <- getRemainingSeq get
    pure $! WavBody pre sam post
  put (WavBody pre sam post) = do
    putSeq put pre
    put sam
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
    let !fmt = chunkValue (unWavFormatChunk fmtChunk)
        !fmtBps = fromIntegral (wfBitsPerSample fmt)
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
    let !fmt = chunkValue (unWavFormatChunk fmtChunk)
        !bps = wfBitsPerSample fmt
    case getSampled bps of
      Nothing -> fail ("Bad bps: " ++ show bps)
      Just (Sampled prox) -> fmap (SampledWav . Sampled) (getRestOfWav prox remainingSize fmtChunk)
  put (SampledWav (Sampled wd)) = put wd
