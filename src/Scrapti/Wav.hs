{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scrapti.Wav
  ( Sampled (..)
  , WavFormatBody (..)
  , WavFormatChunk
  , WavHeader (..)
  , WavDataBody (..)
  , WavDataChunk
  , WavUnparsedBody (..)
  , WavUnparsedChunk
  , WavInfoElem (..)
  , WavInfoChunk
  , WavAdtlData (..)
  , WavAdtlElem (..)
  , WavAdtlChunk
  , WavCuePoint (..)
  , WavCueBody (..)
  , WavCueChunk
  , WavExtraChunk (..)
  , WavChoiceChunk (..)
  , WavBody (..)
  , Wav (..)
  , SampledWav (..)
  , labelWave
  , bindWavExtra
  , filterWavExtra
  , appendWavExtra
  , WavDspErr (..)
  , monoWav
  , setWavInfo
  , SimpleCuePoint (..)
  , toSimpleCuePoint
  , fromSimpleCuePoint
  ) where

import Control.Exception (Exception)
import Control.Monad (join, unless)
import Control.Monad.Identity (Identity (..))
import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, Int16LE, Int32LE, PrimArray, ShortByteString,
               StaticByteSized (..), Word16LE, Word32LE, byteSizeFoldable, getByteString, getExact, getRemainingSeq,
               getRemainingStaticArray, getRemainingString, getSeq, getUnfold, putByteString, putSeq, putStaticArray)
import Data.Default (Default (..))
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Scrapti.Dsp (DspErr, Sel, monoFromSel)
import Scrapti.Riff (Chunk (..), ChunkLabel (..), KnownChunk (..), KnownLabel (..), KnownListChunk (..), Label,
                     chunkHeaderSize, countSize, getChunkSize, getExpectLabel, labelRiff, labelSize, peekChunkLabel,
                     putChunkSize)

labelWave, labelFmt, labelData, labelInfo, labelAdtl, labelCue, labelNote, labelLabl, labelLtxt :: Label
labelWave = "WAVE"
labelFmt = "fmt "
labelData = "data"
labelInfo = "INFO"
labelAdtl = "adtl"
labelCue = "cue "
labelNote = "note"
labelLabl = "labl"
labelLtxt = "ltxt"

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

type WavFormatChunk = KnownChunk WavFormatBody

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

type WavDataChunk a = KnownChunk (WavDataBody a)

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

type WavUnparsedChunk = Chunk WavUnparsedBody

data WavInfoElem = WavInfoElem
  { wieKey :: !Label
  , wieVal :: !ShortByteString
  } deriving stock (Eq, Show)

instance ByteSized WavInfoElem where
  byteSize (WavInfoElem _ val) = labelSize + countSize + byteSize val

instance Binary WavInfoElem where
  get = do
    key <- get
    sz <- getChunkSize
    val <- getByteString sz
    pure $! WavInfoElem key val
  put (WavInfoElem key val) = do
    put key
    putChunkSize (byteSize val)
    putByteString val

instance KnownLabel WavInfoElem where
  knownLabel _ = labelInfo

type WavInfoChunk = KnownListChunk WavInfoElem

-- NOTE: these are all the same for now, but ltxt has additional
-- structure that could be parsed out later
data WavAdtlData =
    WavAdtlDataLabl !ShortByteString
  | WavAdtlDataNote !ShortByteString
  | WavAdtlDataLtxt !ShortByteString
  deriving stock (Eq, Show)

instance ByteSized WavAdtlData where
  byteSize = \case
    WavAdtlDataLabl bs -> byteSize bs
    WavAdtlDataNote bs -> byteSize bs
    WavAdtlDataLtxt bs -> byteSize bs

data WavAdtlElem = WavAdtlElem
  { waeCueId :: !Word32LE
  , waeData :: !WavAdtlData
  } deriving stock (Eq, Show)

instance ByteSized WavAdtlElem where
  byteSize (WavAdtlElem _ dat) = 4 + countSize + byteSize dat

instance Binary WavAdtlElem where
  get = do
    lab <- get
    sz <- getChunkSize
    (cueId, bs) <- getExact sz $ do
      cueId <- get
      bs <- getRemainingString
      pure (cueId, bs)
    dat <- if
      | lab == labelNote -> pure $! WavAdtlDataNote bs
      | lab == labelLabl -> pure $! WavAdtlDataLabl bs
      | lab == labelLtxt -> pure $! WavAdtlDataLtxt bs
      | otherwise -> fail ("Unknown adtl sub-chunk: " ++ show lab)
    pure $! WavAdtlElem cueId dat
  put (WavAdtlElem cueId dat) = do
    put $! case dat of
      WavAdtlDataLabl _ -> labelLabl
      WavAdtlDataNote _ -> labelNote
      WavAdtlDataLtxt _ -> labelLtxt
    putChunkSize (byteSize dat)
    put cueId
    putByteString $! case dat of
      WavAdtlDataLabl bs -> bs
      WavAdtlDataNote bs -> bs
      WavAdtlDataLtxt bs -> bs

instance KnownLabel WavAdtlElem where
  knownLabel _ = labelAdtl

type WavAdtlChunk = KnownListChunk WavAdtlElem

data WavCuePoint = WavCuePoint
  { wcpPointId :: !Word32LE
  , wcpPosition :: !Word32LE
  , wcpChunkId :: !Word32LE
  , wcpChunkStart :: !Word32LE
  , wcpBlockStart :: !Word32LE
  , wcpSampleStart :: !Word32LE
  } deriving stock (Eq, Show)

cuePointSize :: ByteCount
cuePointSize = 24

instance ByteSized WavCuePoint where
  byteSize _ = cuePointSize

instance StaticByteSized WavCuePoint where
  staticByteSize _ = cuePointSize

instance Binary WavCuePoint where
  get = do
    wcpPointId <- get
    wcpPosition <- get
    wcpChunkId <- get
    wcpChunkStart <- get
    wcpBlockStart <- get
    wcpSampleStart <- get
    pure $! WavCuePoint {..}
  put (WavCuePoint {..}) = do
    put wcpPointId
    put wcpPosition
    put wcpChunkId
    put wcpChunkStart
    put wcpBlockStart
    put wcpSampleStart

newtype WavCueBody = WavCueBody
  { wcbPoints :: Seq WavCuePoint
  } deriving stock (Eq, Show)

instance ByteSized WavCueBody where
  byteSize (WavCueBody points) = countSize + fromIntegral (Seq.length points) * cuePointSize

instance Binary WavCueBody where
  get = do
    count <- get @Word32LE
    points <- getSeq (fromIntegral count) get
    pure $! WavCueBody points
  put (WavCueBody points) = do
    put (fromIntegral (Seq.length points) :: Word32LE)
    putSeq put points

instance KnownLabel WavCueBody where
  knownLabel _ = labelCue

type WavCueChunk = KnownChunk WavCueBody

data WavExtraChunk =
    WavExtraChunkUnparsed !(Chunk WavUnparsedBody)
  | WavExtraChunkInfo !WavInfoChunk
  | WavExtraChunkAdtl !WavAdtlChunk
  | WavExtraChunkCue !WavCueChunk
  deriving stock (Eq, Show)

instance ByteSized WavExtraChunk where
  byteSize = \case
    WavExtraChunkUnparsed x -> byteSize x
    WavExtraChunkInfo x -> byteSize x
    WavExtraChunkAdtl x -> byteSize x
    WavExtraChunkCue x -> byteSize x

getExtra :: ChunkLabel -> Get WavExtraChunk
getExtra = \case
  ChunkLabelList label | label == labelInfo -> fmap WavExtraChunkInfo get
  ChunkLabelList label | label == labelAdtl -> fmap WavExtraChunkAdtl get
  ChunkLabelSingle label | label == labelCue -> fmap WavExtraChunkCue get
  _ -> fmap WavExtraChunkUnparsed get

instance Binary WavExtraChunk where
  get = peekChunkLabel >>= getExtra
  put = \case
    WavExtraChunkUnparsed x -> put x
    WavExtraChunkInfo x -> put x
    WavExtraChunkAdtl x -> put x
    WavExtraChunkCue x -> put x

data WavChoiceChunk a =
    WavChoiceChunkExtra !WavExtraChunk
  | WavChoiceChunkData !(WavDataChunk a)
  deriving stock (Eq, Show)

instance (Prim a, StaticByteSized a) => ByteSized (WavChoiceChunk a) where
  byteSize = \case
    WavChoiceChunkExtra wce -> byteSize wce
    WavChoiceChunkData wcd -> byteSize wcd

instance (Prim a, StaticByteSized a) => Binary (WavChoiceChunk a) where
  get = do
    chunkLab <- peekChunkLabel
    case chunkLab of
      ChunkLabelSingle label | label == labelData -> fmap WavChoiceChunkData get
      _ -> fmap WavChoiceChunkExtra (getExtra chunkLab)
  put = \case
    WavChoiceChunkExtra wce -> put wce
    WavChoiceChunkData wcd -> put wcd

data WavBody a = WavBody
  { wbUnparsedPre :: !(Seq WavExtraChunk)
  , wbSample :: !(KnownChunk (WavDataBody a))
  , wbUnparsedPost :: !(Seq WavExtraChunk)
  } deriving stock (Eq, Show)

instance Default (WavBody a) where
  def = WavBody Empty def Empty

instance (Prim a, StaticByteSized a) => ByteSized (WavBody a) where
  byteSize (WavBody pre sam post) = byteSizeFoldable pre + byteSize sam + byteSizeFoldable post

instance (Prim a, StaticByteSized a) => Binary (WavBody a) where
  get = do
    (!pre, !dat) <- getUnfold Empty $ \pre -> do
      choice <- get
      pure $! case choice of
        WavChoiceChunkExtra xtra -> Left (pre :|> xtra)
        WavChoiceChunkData dat -> Right (pre, dat)
    post <- getRemainingSeq get
    pure $! WavBody pre dat post
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
    let !fmt = knownChunkBody fmtChunk
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
    let !fmt = knownChunkBody fmtChunk
        !bps = wfbBitsPerSample fmt
    case getSampled bps of
      Nothing -> fail ("Bad bps: " ++ show bps)
      Just (Sampled prox) -> fmap (SampledWav . Sampled) (getRestOfWav prox remainingSize fmtChunk)
  put (SampledWav (Sampled wd)) = put wd

bindWavExtraM :: Applicative m => (WavExtraChunk -> m (Seq WavExtraChunk)) -> WavBody a -> m (WavBody a)
bindWavExtraM f (WavBody pre dat post) = (`WavBody` dat) <$> fmap join (traverse f pre) <*> fmap join (traverse f post)

bindWavExtra :: (WavExtraChunk -> Seq WavExtraChunk) -> WavBody a -> WavBody a
bindWavExtra f = runIdentity . bindWavExtraM (Identity . f)

filterWavExtra :: (WavExtraChunk -> Bool) -> WavBody a -> WavBody a
filterWavExtra p = bindWavExtra (\c -> if p c then Seq.singleton c else Empty)

appendWavExtra :: WavExtraChunk -> WavBody a -> WavBody a
appendWavExtra c (WavBody pre dat post) = WavBody pre dat (post :|> c)

data WavDspErr =
    WavDspErrEmbed !DspErr
  | WavDspErrCannotMono !Word16LE
  deriving stock (Eq, Show)

instance Exception WavDspErr

monoWav :: Prim a => Sel a -> Wav a -> Either WavDspErr (Wav a)
monoWav sel (Wav (KnownChunk fmt) (WavBody pre (KnownChunk (WavDataBody arr)) post)) =
  case wfbNumChannels fmt of
    2 -> case monoFromSel sel arr of
      Left dspErr -> Left (WavDspErrEmbed dspErr)
      Right arr' ->
        let fmt' = fmt { wfbNumChannels = 1 }
        in Right (Wav (KnownChunk fmt') (WavBody pre (KnownChunk (WavDataBody arr')) post))
    x -> Left (WavDspErrCannotMono x)

setWavInfo :: WavInfoChunk -> WavBody a -> WavBody a
setWavInfo info = appendWavExtra (WavExtraChunkInfo info) . filterWavExtra (not . isInfo) where
  isInfo = \case { WavExtraChunkInfo _ -> True; _ -> False }

data SimpleCuePoint = SimpleCuePoint
  { scpPointId :: !Word32LE
  , scpSampleStart :: !Word32LE
  } deriving stock (Eq, Show)

toSimpleCuePoint :: WavCuePoint -> SimpleCuePoint
toSimpleCuePoint wcp = SimpleCuePoint (wcpPointId wcp) (wcpSampleStart wcp)

fromSimpleCuePoint :: SimpleCuePoint -> WavCuePoint
fromSimpleCuePoint scp = WavCuePoint (scpPointId scp) (scpSampleStart scp) 0 0 0 (scpSampleStart scp)
