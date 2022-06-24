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
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (Default (..))
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Vector.Primitive as VP
import Scrapti.Binary (Binary (..), BinaryParser (..), ByteLength, ByteSized (..), Int16LE, Int32LE, Int64LE, ParseM,
                       StaticByteSized (..), WithByteSize (..), Word16LE, Word32LE (..), getWithoutSize, parseBound,
                       parseByteString, parseRemaining, parseRepeated, parseUnfold, parseVecRemaining, parseWithSize,
                       putByteString, putSeq, putVec)
import Scrapti.Riff (Chunk (..), Label, StaticLabel (..), labelRiff, parseChunkSize, parseExpectLabel, putChunkSize)

labelWave, labelFmt, labelData :: Label
labelWave = "WAVE"
labelFmt = "fmt "
labelData = "data"

newtype BitLength = BitLength { unBitLength :: Word16LE }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default, Binary, ByteSized)

instance BinaryParser BitLength

data Sampled f where
  Sampled :: (Prim a, Binary a, StaticByteSized a) => !(f a) -> Sampled f

getSampled :: BitLength -> Maybe (Sampled Proxy)
getSampled = \case
  8 -> Just (Sampled (Proxy :: Proxy Int8))
  16 -> Just (Sampled (Proxy :: Proxy Int16LE))
  -- 24 -> Just (Sampled (Proxy :: Proxy Int24LE))
  32 -> Just (Sampled (Proxy :: Proxy Int32LE))
  64 -> Just (Sampled (Proxy :: Proxy Int64LE))
  _ -> Nothing

data WavFormat = WavFormat
  { wfFormatType :: !Word16LE
  , wfNumChannels :: !Word16LE
  , wfSampleRate :: !Word32LE
  , wfBitsPerSample :: !BitLength
  , wfExtra :: !ByteString
  } deriving stock (Eq, Show)

instance Default WavFormat where
  def = WavFormat 1 2 44100 16 BS.empty

instance Binary WavFormat where
  get = getWithoutSize
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

instance ByteSized WavFormat where
  byteSize wf = 16 + fromIntegral (BS.length (wfExtra wf))

isSupportedBPS :: BitLength -> Bool
isSupportedBPS w = mod w 8 == 0 && w <= 64

isSupportedFmtExtraSize :: ByteLength -> Bool
isSupportedFmtExtraSize x = x == 0 || x == 2 || x == 24

instance BinaryParser WavFormat where
  parseWithoutSize = do
    formatType <- parseWithoutSize
    numChannels <- parseWithoutSize
    sampleRate <- parseWithoutSize
    bpsAvg <- parseWithoutSize
    bpsSlice <- parseWithoutSize
    bps <- parseWithoutSize
    unless (isSupportedBPS bps) (fail ("bad bps: " ++ show bps))
    unless (bpsSlice == div (fromIntegral bps) 8 * numChannels) (fail ("bad bps slice: " ++ show bpsSlice))
    unless (bpsAvg == sampleRate * fromIntegral bpsSlice) (fail ("bad average bps: " ++ show bpsAvg))
    extra <- parseRemaining
    let !extraLen = fromIntegral (BS.length extra)
    unless (isSupportedFmtExtraSize extraLen) (fail ("bad extra length: " ++ show extraLen))
    pure $! WavFormat formatType numChannels sampleRate bps extra

instance StaticLabel WavFormat where
  staticLabel = const labelFmt

newtype WavFormatChunk = WavFormatChunk { unWavFormatChunk :: Chunk WavFormat }
  deriving stock (Show)
  deriving newtype (Eq, Binary, ByteSized, Default)

instance BinaryParser WavFormatChunk

newtype WavSampleChunk a = WavSampleChunk { unWavSampleChunk :: VP.Vector a }
  deriving stock (Show)
  deriving newtype (Eq)

instance (Prim a, StaticByteSized a) => ByteSized (WavSampleChunk a) where
  byteSize (WavSampleChunk vec) = 8 + byteSize vec

instance (Prim a, StaticByteSized a, Binary a) => Binary (WavSampleChunk a) where
  get = getWithoutSize
  put (WavSampleChunk vec) = do
    put labelData
    let !chunkSize = byteSize vec
    putChunkSize chunkSize
    putVec vec

parseSampleChunkPostLabel :: (Prim a, StaticByteSized a, Binary a) => ParseM (WavSampleChunk a)
parseSampleChunkPostLabel = do
  chunkSize <- parseChunkSize
  vec <- parseBound chunkSize (parseVecRemaining (Proxy :: Proxy a))
  pure $! WavSampleChunk vec

instance (Prim a, StaticByteSized a, Binary a) => BinaryParser (WavSampleChunk a) where
  parseWithoutSize = do
    parseExpectLabel labelData
    parseSampleChunkPostLabel

instance Prim a => Default (WavSampleChunk a) where
  def = WavSampleChunk VP.empty

data WavHeader = WavHeader
  { wavHeaderRemainingSize :: !ByteLength
  , wavHeaderFormat :: !WavFormatChunk
  } deriving stock (Eq, Show)

instance Binary WavHeader where
  get = getWithoutSize
  put (WavHeader remainingSize format) = do
    let !formatSize = byteSize format
        !fileSize = remainingSize + formatSize + 4
    put labelRiff
    putChunkSize fileSize
    put labelWave
    put format

instance ByteSized WavHeader where
  byteSize (WavHeader _ format) = 12 + byteSize format

instance BinaryParser WavHeader where
  parseWithoutSize = do
    parseExpectLabel labelRiff
    fileSize <- parseChunkSize
    parseExpectLabel labelWave
    WithByteSize formatSize format <- parseWithSize
    let !remainingSize = fileSize - formatSize - 4
    pure $! WavHeader remainingSize format

data WavUnparsedChunk = WavUnparsedChunk
  { wavUnparsedLabel :: !Label
  , wavUnparsedContents :: !ByteString
  } deriving stock (Eq, Show)

instance ByteSized WavUnparsedChunk where
  byteSize (WavUnparsedChunk _ bs) = 8 + fromIntegral (BS.length bs)

parseUnparsedChunkPostLabel :: Label -> ParseM WavUnparsedChunk
parseUnparsedChunkPostLabel label = do
  chunkSize <- parseChunkSize
  bs <- parseByteString chunkSize
  pure $! WavUnparsedChunk label bs

instance Binary WavUnparsedChunk where
  get = getWithoutSize
  put (WavUnparsedChunk label bs) = do
    put label
    putChunkSize (fromIntegral (BS.length bs))
    putByteString bs

instance BinaryParser WavUnparsedChunk where
  parseWithoutSize = do
    label <- parseWithoutSize
    parseUnparsedChunkPostLabel label

data WavChunk a =
    WavChunkUnparsed !WavUnparsedChunk
  | WavChunkSample !(WavSampleChunk a)
  deriving stock (Eq, Show)

instance (Prim a, StaticByteSized a) => ByteSized (WavChunk a) where
  byteSize = \case
    WavChunkUnparsed wuc -> byteSize wuc
    WavChunkSample wsc -> byteSize wsc

instance (Prim a, StaticByteSized a, Binary a) => Binary (WavChunk a) where
  get = getWithoutSize
  put = \case
    WavChunkUnparsed wuc -> put wuc
    WavChunkSample wsc -> put wsc

instance (Prim a, StaticByteSized a, Binary a) => BinaryParser (WavChunk a) where
  parseWithoutSize = do
    label <- parseWithoutSize @Label
    if label == labelData
      then fmap WavChunkSample parseSampleChunkPostLabel
      else fmap WavChunkUnparsed (parseUnparsedChunkPostLabel label)

data WavBody a = WavBody
  { wbUnparsedPre :: !(Seq WavUnparsedChunk)
  , wbSample :: !(WavSampleChunk a)
  , wbUnparsedPost :: !(Seq WavUnparsedChunk)
  } deriving stock (Eq, Show)

instance Prim a => Default (WavBody a) where
  def = WavBody Empty def Empty

instance (Prim a, StaticByteSized a) => ByteSized (WavBody a) where
  byteSize (WavBody pre sam post) = byteSize pre + byteSize sam + byteSize post

instance (Prim a, StaticByteSized a, Binary a) => Binary (WavBody a) where
  get = getWithoutSize
  put (WavBody pre sam post) = do
    putSeq pre
    put sam
    putSeq post

instance (Prim a, StaticByteSized a, Binary a) => BinaryParser (WavBody a) where
  parseWithoutSize = do
    (!pre, !sam) <- parseUnfold Empty $ \pre -> do
      chunk <- parseWithoutSize
      pure $! case chunk of
        WavChunkUnparsed wuc -> Left (pre :|> wuc)
        WavChunkSample wsc -> Right (pre, wsc)
    post <- parseRepeated @WavUnparsedChunk
    pure $! WavBody pre sam post

data Wav a = Wav
  { wavFormat :: !WavFormatChunk
  , wavBody :: !(WavBody a)
  } deriving stock (Eq, Show)

instance Prim a => Default (Wav a) where
  def = Wav def def

instance (Prim a, StaticByteSized a) => ByteSized (Wav a) where
  byteSize (Wav fmt body) =
    let !remainingSize = byteSize body
        !header = WavHeader remainingSize fmt
    in byteSize header + byteSize body

instance (Prim a, StaticByteSized a, Binary a) => Binary (Wav a) where
  get = getWithoutSize
  put (Wav fmtChunk body) = do
    let !remainingSize = byteSize body
        !header = WavHeader remainingSize fmtChunk
    put header
    put body

parseRestOfWav :: (Prim a, Binary a, StaticByteSized a) => Proxy a -> ByteLength -> WavFormatChunk -> ParseM (Wav a)
parseRestOfWav _ remainingSize fmtChunk = do
  parseBound remainingSize $ do
    (!pre, !sam) <- parseUnfold Empty $ \pre -> do
      chunk <- parseWithoutSize
      pure $! case chunk of
        WavChunkUnparsed wuc -> Left (pre :|> wuc)
        WavChunkSample wsc -> Right (pre, wsc)
    post <- parseRepeated @WavUnparsedChunk
    let body = WavBody pre sam post
    pure $! Wav fmtChunk body

instance (Prim a, StaticByteSized a, Binary a) => BinaryParser (Wav a) where
  parseWithoutSize = do
    WavHeader remainingSize fmtChunk <- parseWithoutSize
    let !fmt = chunkValue (unWavFormatChunk fmtChunk)
        !fmtBps = fromIntegral (wfBitsPerSample fmt)
        !prox = Proxy :: Proxy a
        !parseBps = staticByteSize prox
    unless (fmtBps == parseBps) (fail ("bad bps: in header: " ++ show fmtBps ++ " required: " ++ show parseBps))
    parseRestOfWav prox remainingSize fmtChunk

newtype SampledWav = SampledWav { unSampledWav :: Sampled Wav }

instance ByteSized SampledWav where
  byteSize (SampledWav (Sampled wd)) = byteSize wd

instance Binary SampledWav where
  get = getWithoutSize
  put (SampledWav (Sampled wd)) = put wd

instance BinaryParser SampledWav where
  parseWithoutSize = do
    WavHeader remainingSize fmtChunk <- parseWithoutSize
    let !fmt = chunkValue (unWavFormatChunk fmtChunk)
        !bps = wfBitsPerSample fmt
    case getSampled bps of
      Nothing -> fail ("bad bps: " ++ show bps)
      Just (Sampled prox) -> fmap (SampledWav . Sampled) (parseRestOfWav prox remainingSize fmtChunk)
