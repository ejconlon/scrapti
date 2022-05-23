{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Wav where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word16, Word32, Word8)
import Scrapti.Binary (ByteOffset, DecodeResult, DecodeSuccess (..), Get, Put, decode, getByteString, getExpect,
                       getInt16le, getInt32le, getInt64le, getInt8, getVec, getWord16le, getWord32le, putInt16le,
                       putInt32le, putInt64le, putInt8, skip)

-- import Data.Store (Get, Put, Store (..), decodeIOPortionWith, decodeIO, decodeIOWith)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Unboxed as UV
import GHC.IO (unsafePerformIO)

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

data Wav a = Wav
  { wavFormat :: !WavFormat
  , wavData :: !(WavData a)
  } deriving stock (Eq, Show)

decodeWavHeader :: BSL.ByteString -> DecodeResult WavHeader
decodeWavHeader bs = fmap (\(DecodeSuccess off (toSkip, header)) -> DecodeSuccess (off + fromIntegral toSkip) header) (decode bs getWavHeader)

decodeContinuedWavData :: VU.Unbox a => BSL.ByteString -> ByteOffset -> Word16 -> Get a -> DecodeResult (WavData a)
decodeContinuedWavData bs offset bps getter = decode (BSL.drop offset bs) (getContinuedWavData bps getter)

decodeWav :: BSL.ByteString -> DecodeResult (Sampled Wav)
decodeWav bs = decode bs getWav

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

getWavHeader :: Get (Int, WavHeader)
getWavHeader = do
  expectLabel labelRiff
  fileSize <- getWord32le
  expectLabel labelWave
  expectLabel labelFmt
  (toSkip, format) <- getWavFormat
  pure (toSkip, WavHeader fileSize format)

isSupportedBPS :: Word16 -> Bool
isSupportedBPS w = mod w 8 == 0 && w <= 64

getWavFormat :: Get (Int, WavFormat)
getWavFormat = do
  chunkSize <- getWord32le
  _ <- expectCode 1
  numChannels <- getWord16le
  sampleRate <- getWord32le
  bpsAvg <- getWord32le
  bpsSlice <- getWord16le
  bps <- getWord16le
  unless (bpsAvg == sampleRate * fromIntegral bpsSlice) (fail "bad average bps")
  unless (isSupportedBPS bps) (fail "bad bps")
  unless (bpsSlice == div bps 8 * numChannels) (fail "bad bps slice")
  let toSkip = fromIntegral chunkSize - 16
  pure (toSkip, WavFormat numChannels sampleRate bps)

getContinuedWavData :: VU.Unbox a => Word16 -> Get a -> Get (WavData a)
getContinuedWavData bps geter = go where
  go = do
    lab <- getLabel
    if lab == labelData
      then getWavData bps geter
      else getUnknownData *> go

getWavData :: VU.Unbox a => Word16 -> Get a -> Get (WavData a)
getWavData bitsPer geter = do
  let bytesPer = div bitsPer 8
  chunkSize <- getWord32le
  unless (mod chunkSize (fromIntegral bytesPer) == 0) (fail "bad data chunk size")
  let samples = fromIntegral (div chunkSize (fromIntegral bytesPer))
  vec <- getVec samples geter
  pure (WavData vec)

getUnknownData :: Get ()
getUnknownData = do
  chunkSize <- getWord32le
  skip (fromIntegral chunkSize)

getWav :: Get (Sampled Wav)
getWav = do
  (toSkip, WavHeader _ fmt) <- getWavHeader
  skip toSkip
  let bps = wfBitsPerSample fmt
  case getSampled bps of
    Nothing -> fail "bad bps"
    Just (Sampled geter) -> do
      dat <- getContinuedWavData bps geter
      pure (Sampled (Wav fmt dat))

-- -- All numerical values are stored in little endian format
-- --
-- parseWav :: (MArray IOUArray a IO, IArray UArray a, Audible a, AudibleInWav a) => Parser (Audio a)
-- parseWav = do
--   _ <- string "RIFF"
-- --  n <- remaining
-- --  expect (\w -> fromIntegral w ==  n - 4) GetWord32le
--   _ <- GetWord32le -- chunkSize
--   _ <- string "WAVE"
--   _ <- many parseUnknownChunk
--   (sampleRate1,channelNumber1,bitsPerSample1) <- parseFmt
--   _ <- many parseUnknownChunk
--   sampleData1 <- parseData channelNumber1 bitsPerSample1
--   return $! (Audio sampleRate1 channelNumber1 sampleData1)

-- buildWav :: (IArray UArray a, Audible a, AudibleInWav a) => Audio a -> Builder
-- buildWav a = mconcat [
--     PutString "RIFF"
--   , PutWord32le $ fromIntegral chunkSize
--   , PutString "WAVE"
--   , buildFmt a
--   , buildData a]
--   where
--   sd = sampleData a
--   chunkSize =
--       4  -- "WAVE"
--     + 24 -- fmt chunk
--     + 8  -- data chunk header
--     + (fromIntegral $ sampleNumber sd) * (bytesPerSample $ sampleType sd)
--        -- sample data

-- parseFmt :: Parser (Int,Int,Int)
-- parseFmt = do
--   _ <- string "fmt "
--   chunkSize <- GetWord32le >>= return . fromIntegral
--   _ <- word16le 1 -- compression code
--   channelNumber1 <- GetWord16le >>= return . fromIntegral
--   sampleRate1 <- GetWord32le >>= return . fromIntegral
--   avgBytesPerSec <- GetWord32le >>= return . fromIntegral
--   bytesPerSampleSlice <- GetWord16le >>= return . fromIntegral
--   when (avgBytesPerSec /= sampleRate1 * bytesPerSampleSlice) $
--     fail "avgBytesPerSec /= sampleRate * bytesPerSampleSlise"
--   bitsPerSample1 <- expect (\w -> (mod w 8 == 0) && w <= 64) GetWord16le >>= return . fromIntegral
--   when (bytesPerSampleSlice /= (div bitsPerSample1 8) * channelNumber1) $
--     fail "bytesPerSampleSlice /= (div bitsPerSample 8) * channelNumber"
--   skip (chunkSize - 16) -- skip extra fromat bytes
--   return $! (sampleRate1,channelNumber1,bitsPerSample1)

-- buildFmt :: (IArray UArray a, Audible a, AudibleInWav a) => Audio a -> Builder
-- buildFmt a = mconcat [
--     PutString   $ "fmt "
--   , PutWord32le $ 16 -- chunk size
--   , PutWord16le $ 1  -- compression code
--   , PutWord16le $ fromIntegral $ channelNumber a
--   , PutWord32le $ fromIntegral $ sampleRate a
--   , PutWord32le $ fromIntegral $ avgBytesPerSec
--   , PutWord16le $ fromIntegral $ bytesPerSampleSlice
--   , PutWord16le $ fromIntegral $ bitsPS
--   ]
--   where
--   sd = sampleData a
--   bitsPS = bitsPerSample $ sampleType sd
--   bytesPS = bytesPerSample $ sampleType sd
--   bytesPerSampleSlice = bytesPS * channelNumber a
--   avgBytesPerSec = sampleRate a * bytesPerSampleSlice

-- parseData :: (MArray IOUArray a IO, IArray UArray a, Audible a, AudibleInWav a)
--   => Int -> Int -> Parser (SampleData a)
-- parseData cn bitsPS = do
--   _ <- string "data"
--   let bytesPS = div bitsPS 8
--   chunkSize <- expect (\w -> mod (fromIntegral w) bytesPS == 0) GetWord32le
--                >>= return . fromIntegral
--   let sn = fromIntegral $ div chunkSize bytesPS
--   when (mod sn (fromIntegral cn) /= 0) $ fail "mod sampelNumber channelNumber /= 0)"
--   parseSampleData sn (parserSelector bitsPS)

-- buildData :: (IArray UArray a, Audible a, AudibleInWav a) => Audio a -> Builder
-- buildData a = mconcat [
--     PutString "data"
--   , PutWord32le $ fromIntegral $ chunkSize
--   , buildSampleData buildSample sd]
--   where
--   sd = sampleData a
--   chunkSize = (fromIntegral $ sampleNumber sd) * (bytesPerSample $ sampleType sd)

-- parseUnknownChunk :: Parser ()
-- parseUnknownChunk = do
--   _ <- expect (\s -> s /= "data" && s /= "fmt ") (GetString 4)
--   chunkSize <- GetWord32le
--   skip(fromIntegral chunkSize)
--   return ()
