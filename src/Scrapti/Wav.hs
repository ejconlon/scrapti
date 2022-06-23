{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Wav
  ( Sampled (..)
  , WavFormat (..)
  , WavData (..)
  , WavHeader (..)
  , WavUnparsed (..)
  , WavChunk (..)
  , Wav (..)
  , wavDataSamples
  , decodeWavHeader
  , decodeWavChunk
  , decodeWavTrailers
  , decodeAnyWav
  , decodeSpecificWav
  , encodeAnyWav
  , encodeSpecificWav
  ) where

import Control.Monad (unless)
import Control.Monad.State.Strict (gets)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Default (Default (..))
import Data.Foldable (foldMap', for_)
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Proxy (Proxy)
import Data.Semigroup (Sum (..))
import Data.Sequence (Seq (..))
import qualified Data.Vector.Primitive as VP
import Scrapti.Binary (Binary (..), ByteLength, DecodeState (..), DecodeT, Get, Int16LE, Int32LE, Int64LE, Put,
                       SizedBinary, Word16LE, Word32LE (..), decodeBounded, decodeGet, getByteString, getExpect, getVec,
                       guardEnd, putByteString, putVec, runPut, skip)
import Scrapti.Riff (expectLabel, getChunkSize, getLabel, labelRiff, putChunkSize)

newtype BitLength = BitLength { unBitLength :: Word16LE }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default, Binary, SizedBinary)

data Sampled f where
  Sampled :: (Prim a, SizedBinary a) => !(f a) -> Sampled f

getSampled :: BitLength -> Maybe (Sampled Get)
getSampled = \case
  8 -> Just (Sampled (get :: Get Int8))
  16 -> Just (Sampled (get :: Get Int16LE))
  -- 24 -> Just (Sampled (get :: Get Int24LE))
  32 -> Just (Sampled (get :: Get Int32LE))
  64 -> Just (Sampled (get :: Get Int64LE))
  _ -> Nothing

data WavFormat = WavFormat
  { wfNumChannels :: !Word16LE
  , wfSampleRate :: !Word32LE
  , wfBitsPerSample :: !BitLength
  } deriving stock (Eq, Show)

instance Default WavFormat where
  def = WavFormat 2 44100 16

newtype WavData a = WavData { unWavData :: VP.Vector a }
  deriving stock (Show)
  deriving newtype (Eq)

instance Prim a => Default (WavData a) where
  def = WavData VP.empty

data WavHeader = WavHeader
  { wavHeaderRemainingSize :: !ByteLength
  , wavHeaderFormat :: !WavFormat
  } deriving stock (Eq, Show)

data WavUnparsed = WavUnparsed
  { wavUnparsedLabel :: !ByteString
  , wavUnparsedContents :: !ByteString
  } deriving stock (Eq, Show)

data WavBody a = WavBody
  { wbMiddle :: !(Seq WavUnparsed)
  , wbData :: !(WavData a)
  } deriving stock (Eq, Show)

data Wav a = Wav
  { wavFormat :: !WavFormat
  , wavMiddle :: !(Seq WavUnparsed)
  , wavData :: !(WavData a)
  , wavTrailer :: !(Seq WavUnparsed)
  } deriving stock (Eq, Show)

instance Prim a => Default (Wav a) where
  def = Wav def Empty def Empty

data WavChunk a =
    WavChunkUnparsed !WavUnparsed
  | WavChunkData !(WavData a)
  deriving stock (Eq, Show)

wavDataSamples :: Prim a => WavData a -> Int
wavDataSamples = VP.length . unWavData

decodeWavHeader :: Monad m => DecodeT m WavHeader
decodeWavHeader = decodeGet getWavHeader

decodeWavChunk :: (Monad m, Prim a) => BitLength -> Get a -> DecodeT m (WavChunk a)
decodeWavChunk bps getter = decodeGet (getWavChunk bps getter)

decodeWavTrailers :: Monad m => DecodeT m (Seq WavUnparsed)
decodeWavTrailers = go Empty where
  go !acc = do
    bs <- gets decStateInput
    if BSL.null bs
      then pure acc
      else do
        unp <- decodeGet (getLabel >>= getWavUnparsed)
        go (acc :|> unp)

decodeAnyWav :: Monad m => DecodeT m (Sampled Wav)
decodeAnyWav = do
  WavHeader remainingSize fmt <- decodeWavHeader
  case getSampled (wfBitsPerSample fmt) of
    Nothing -> fail "bad bps"
    Just (Sampled getter) -> fmap Sampled (decodeRestOfWav remainingSize fmt getter)

decodeSpecificWav :: (Monad m, Prim a, SizedBinary a) => Proxy a -> DecodeT m (Wav a)
decodeSpecificWav _ = do
  WavHeader remainingSize fmt <- decodeWavHeader
  decodeRestOfWav remainingSize fmt get

decodeRestOfWav :: (Monad m, Prim a) => ByteLength -> WavFormat -> Get a -> DecodeT m (Wav a)
decodeRestOfWav remainingSize fmt getter =
  decodeBounded remainingSize $ do
    WavBody mid dat <- decodeGet (getWavBody (wfBitsPerSample fmt) getter)
    tra <- decodeWavTrailers
    guardEnd
    pure $! Wav fmt mid dat tra

encodeAnyWav :: Sampled Wav -> BSL.ByteString
encodeAnyWav (Sampled wav) = encodeSpecificWav wav

encodeSpecificWav :: (Prim a, SizedBinary a) => Wav a -> BSL.ByteString
encodeSpecificWav = runPut . putSpecificWav

labelWave, labelFmt, labelData :: ByteString
labelWave = "WAVE"
labelFmt = "fmt "
labelData = "data"

getWavHeader :: Get WavHeader
getWavHeader = do
  expectLabel labelRiff
  fileSize <- getChunkSize
  expectLabel labelWave
  expectLabel labelFmt
  (format, formatSize) <- getWavFormat
  let remainingSize = fileSize - formatSize - 12
  pure $! WavHeader remainingSize format

isSupportedBPS :: BitLength -> Bool
isSupportedBPS w = mod w 8 == 0 && w <= 64

isSupportedFmtChunkSize :: ByteLength -> Bool
isSupportedFmtChunkSize x = x == 16 || x == 18 || x == 40

getWavFormat :: Get (WavFormat, ByteLength)
getWavFormat = do
  chunkSize <- getChunkSize
  unless (isSupportedFmtChunkSize chunkSize) (fail "bad fmt chunk size")
  _ <- getExpect "compression code" (get @Word16LE) 1
  numChannels <- get
  sampleRate <- get
  bpsAvg <- get
  bpsSlice <- get
  bps <- get
  unless (bpsAvg == sampleRate * fromIntegral bpsSlice) (fail "bad average bps")
  unless (isSupportedBPS bps) (fail "bad bps")
  unless (bpsSlice == div (fromIntegral bps) 8 * numChannels) (fail "bad bps slice")
  skip (fromIntegral (chunkSize - 16))
  let !format = WavFormat numChannels sampleRate bps
  pure (format, chunkSize)

getWavChunk :: Prim a => BitLength -> Get a -> Get (WavChunk a)
getWavChunk bps getter = go where
  go = do
    lab <- getLabel
    if lab == labelData
      then fmap WavChunkData (getWavData bps getter)
      else fmap WavChunkUnparsed (getWavUnparsed lab)

getWavData :: Prim a => BitLength -> Get a -> Get (WavData a)
getWavData bps getter = do
  let !bytesPer = div (fromIntegral bps) 8
  chunkSize <- getChunkSize
  unless (mod chunkSize bytesPer == 0) (fail "bad data chunk size")
  let !samples = fromIntegral (div chunkSize bytesPer)
  vec <- getVec samples getter
  unless (VP.length vec == samples) (fail "bad samples")
  pure $! WavData vec

getWavUnparsed :: ByteString -> Get WavUnparsed
getWavUnparsed lab = do
  chunkSize <- getChunkSize
  contents <- getByteString (fromIntegral chunkSize)
  pure $! WavUnparsed lab contents

getWavBody :: Prim a => BitLength -> Get a -> Get (WavBody a)
getWavBody bps getter = go Empty where
  go !unps = do
    chunk <- getWavChunk bps getter
    case chunk of
      WavChunkData dat -> pure $! WavBody unps dat
      WavChunkUnparsed unp -> go (unps :|> unp)

putSpecificWav :: (Prim a, SizedBinary a) => Wav a -> Put
putSpecificWav (Wav (WavFormat nchan sr bps) mid (WavData vec) tra) = result where
  result = do
    putByteString labelRiff
    putChunkSize fileSize
    putByteString labelWave
    putByteString labelFmt
    putFmt
    for_ mid putUnp
    putByteString labelData
    putChunkSize dataChunkSize
    putVec put vec
    for_ tra putUnp
  putFmt = do
    putChunkSize fmtChunkSize
    put @Word16LE 1
    put nchan
    put sr
    put bpsAvg
    put bpsSlice
    put bps
  putUnp (WavUnparsed lab con) = do
    putByteString lab
    putChunkSize (fromIntegral (BS.length con))
    putByteString con
  unpSize unps = getSum (foldMap' (\(WavUnparsed _ con) -> Sum (8 + fromIntegral (BS.length con))) unps)
  midSize = unpSize mid
  traSize = unpSize tra
  framingSize = 20
  fmtChunkSize = 16
  bytesPer = fromIntegral (div bps 8)
  dataChunkSize = bytesPer * fromIntegral (VP.length vec)
  fileSize = framingSize + fmtChunkSize + midSize + dataChunkSize + traSize
  bpsAvg = sr * fromIntegral bpsSlice
  bpsSlice = div (fromIntegral bps) 8 * nchan
