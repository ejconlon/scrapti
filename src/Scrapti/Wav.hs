{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Wav
  ( Sample (sampleGet, samplePut, sampleBits, sampleBytes)
  , Sampled (..)
  , getSampled
  , WavFormat (..)
  , WavData (..)
  , WavHeader (..)
  , WavUnparsed (..)
  , WavChunk (..)
  , Wav (..)
  , decodeWavHeader
  , decodeWavChunk
  , decodeWavTrailers
  , decodeAnyWav
  , decodeSpecificWav
  , encodeAnyWav
  -- , encodeSpecificWav
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word16, Word32)
import Scrapti.Binary (DecodeState (decStateInput), DecodeT, Get, Put, decodeGet, getByteString, getExpect, getInt16le,
                       getInt32le, getInt64le, getInt8, getVec, getWord16le, getWord32le, guardEnd, putByteString,
                       putInt16le, putInt32le, putInt64le, putInt8, putVec, putWord16le, putWord32le, runPut)

import Control.Monad.State.Strict (gets)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Data.Semigroup (Sum (..))
import Data.Sequence (Seq (..))

class VU.Unbox a => Sample a where
  sampleGet :: Get a
  samplePut :: a -> Put
  sampleBits :: Proxy a -> Int
  sampleBytes :: Proxy a -> Int
  sampleBytes p = div (sampleBits p) 8

instance Sample Int8 where
  sampleGet = getInt8
  samplePut = putInt8
  sampleBits _ = 8

instance Sample Int16 where
  sampleGet = getInt16le
  samplePut = putInt16le
  sampleBits _ = 16

-- instance Sample Int24 where
--   sampleGet = getInt24le
--   samplePut = putInt24le
--   sampleBits _ = 24

instance Sample Int32 where
  sampleGet = getInt32le
  samplePut = putInt32le
  sampleBits _ = 32

instance Sample Int64 where
  sampleGet = getInt64le
  samplePut = putInt64le
  sampleBits _ = 64

data Sampled f where
  Sampled :: Sample a => !(f a) -> Sampled f

getSampled :: Word16 -> Maybe (Sampled Get)
getSampled = \case
  8 -> Just (Sampled (sampleGet :: Get Int8))
  16 -> Just (Sampled (sampleGet :: Get Int16))
  -- 24 -> Just (Sampled (sampleGet :: Get Int24))
  32 -> Just (Sampled (sampleGet :: Get Int32))
  64 -> Just (Sampled (sampleGet :: Get Int64))
  _ -> Nothing

data WavFormat = WavFormat
  { wfNumChannels :: !Word16
  , wfSampleRate :: !Word32
  , wfBitsPerSample :: !Word16
  } deriving stock (Eq, Show)

newtype WavData a = WavData { unWavData :: VU.Vector a }
  deriving stock (Show)
  deriving newtype (Eq)

data WavHeader = WavHeader
  { wavHeaderFileSize :: !Word32
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

data WavChunk a =
    WavChunkUnparsed !WavUnparsed
  | WavChunkData !(WavData a)
  deriving stock (Eq, Show)

decodeWavHeader :: Monad m => DecodeT m WavHeader
decodeWavHeader = decodeGet getWavHeader

decodeWavChunk :: Monad m => VU.Unbox a => Word16 -> Get a -> DecodeT m (WavChunk a)
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
  WavHeader _ fmt <- decodeWavHeader
  case getSampled (wfBitsPerSample fmt) of
    Nothing -> fail "bad bps"
    Just (Sampled getter) -> fmap Sampled (decodeRestOfWav fmt getter)

decodeSpecificWav :: (Monad m, Sample a) => Proxy a -> DecodeT m (Wav a)
decodeSpecificWav _ = do
  WavHeader _ fmt <- decodeWavHeader
  decodeRestOfWav fmt sampleGet

decodeRestOfWav :: (Monad m, Sample a) => WavFormat -> Get a -> DecodeT m (Wav a)
decodeRestOfWav fmt getter = do
  WavBody mid dat <- decodeGet (getWavBody (wfBitsPerSample fmt) getter)
  tra <- decodeWavTrailers
  guardEnd
  pure $! Wav fmt mid dat tra

encodeAnyWav :: Sampled Wav -> BSL.ByteString
encodeAnyWav (Sampled wav) = encodeSpecificWav wav

encodeSpecificWav :: Sample a => Wav a -> BSL.ByteString
encodeSpecificWav = runPut . putSpecificWav

labelRiff, labelWave, labelFmt, labelData :: ByteString
labelRiff = "RIFF"
labelWave = "WAVE"
labelFmt = "fmt "
labelData = "data"

getLabel :: Get ByteString
getLabel = getByteString 4

expectLabel :: ByteString -> Get ()
expectLabel = getExpect "label" getLabel

expectCode :: Word16 -> Get ()
expectCode = getExpect "compression code" getWord16le

expectChunkSize :: Word32 -> Get ()
expectChunkSize = getExpect "chunk size" getWord32le

getWavHeader :: Get WavHeader
getWavHeader = do
  expectLabel labelRiff
  fileSize <- getWord32le
  expectLabel labelWave
  expectLabel labelFmt
  format <- getWavFormat
  pure $! WavHeader fileSize format

isSupportedBPS :: Word16 -> Bool
isSupportedBPS w = mod w 8 == 0 && w <= 64

getWavFormat :: Get WavFormat
getWavFormat = do
  _ <- expectChunkSize 16
  _ <- expectCode 1
  numChannels <- getWord16le
  sampleRate <- getWord32le
  bpsAvg <- getWord32le
  bpsSlice <- getWord16le
  bps <- getWord16le
  unless (bpsAvg == sampleRate * fromIntegral bpsSlice) (fail "bad average bps")
  unless (isSupportedBPS bps) (fail "bad bps")
  unless (bpsSlice == div bps 8 * numChannels) (fail "bad bps slice")
  pure $! WavFormat numChannels sampleRate bps

getWavChunk :: VU.Unbox a => Word16 -> Get a -> Get (WavChunk a)
getWavChunk bps getter = go where
  go = do
    lab <- getLabel
    if lab == labelData
      then fmap WavChunkData (getWavData bps getter)
      else fmap WavChunkUnparsed (getWavUnparsed lab)

getWavData :: VU.Unbox a => Word16 -> Get a -> Get (WavData a)
getWavData bitsPer getter = do
  let bytesPer = div bitsPer 8
  chunkSize <- getWord32le
  unless (mod chunkSize (fromIntegral bytesPer) == 0) (fail "bad data chunk size")
  let samples = fromIntegral (div chunkSize (fromIntegral bytesPer))
  vec <- getVec samples getter
  unless (VU.length vec == samples) (fail "bad samples")
  pure (WavData vec)

getWavUnparsed :: ByteString -> Get WavUnparsed
getWavUnparsed lab = do
  chunkSize <- getWord32le
  contents <- getByteString (fromIntegral chunkSize)
  pure $! WavUnparsed lab contents

getWavBody :: VU.Unbox a => Word16 -> Get a -> Get (WavBody a)
getWavBody bps getter = go Empty where
  go !unps = do
    chunk <- getWavChunk bps getter
    case chunk of
      WavChunkData dat -> pure $! WavBody unps dat
      WavChunkUnparsed unp -> go (unps :|> unp)

putSpecificWav :: Sample a => Wav a -> Put
putSpecificWav (Wav (WavFormat nchan sr bps) mid (WavData vec) tra) = res where
  putFmt = do
    putWord32le fmtChunkSize
    putWord16le 1
    putWord16le nchan
    putWord32le sr
    putWord32le bpsAvg
    putWord16le bpsSlice
    putWord16le bps
  putUnp (WavUnparsed lab con)= do
    putByteString lab
    putWord32le (fromIntegral (BS.length con))
    putByteString con
  res = do
    putByteString labelRiff
    putWord32le fileSize
    putByteString labelWave
    putByteString labelFmt
    putFmt
    for_ mid putUnp
    putByteString labelData
    putWord32le dataChunkSize
    putVec samplePut vec
    for_ tra putUnp
  unpSize unps = getSum (foldMap (\(WavUnparsed _ con) -> Sum (8 + fromIntegral (BS.length con))) unps)
  midSize = unpSize mid
  traSize = unpSize tra
  framingSize = 20
  fmtChunkSize = 16
  bytesPer = fromIntegral (div bps 8)
  dataChunkSize = bytesPer * fromIntegral (VU.length vec)
  fileSize = framingSize + fmtChunkSize + midSize + dataChunkSize + traSize
  bpsAvg = sr * fromIntegral bpsSlice
  bpsSlice = div bps 8 * nchan
