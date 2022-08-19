{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scrapti.Wav
  ( PaddedString (..)
  , WavFormatBody (..)
  , WavFormatChunk
  , WavHeader (..)
  , WavDataBody (..)
  , WavDataChunk
  , WavUnparsedChunk
  , WavInfoElem (..)
  , WavInfoChunk
  , WavAdtlData (..)
  , WavAdtlElem (..)
  , WavAdtlChunk
  , WavCuePoint (..)
  , WavCueBody (..)
  , WavCueChunk
  , WavChunk (..)
  , Wav (..)
  , lookupWavChunk
  , lookupWavFormatChunk
  , lookupWavDataChunk
  , wavToPcmContainer
  , wavFromPcmContainer
  , wavUseMarkers
  , wavAddChunks
  ) where

import Control.Monad (unless)
import Dahdit (Binary (..), ByteCount, ByteSized (..), ShortByteString, StaticByteSized (..), ViaStaticByteSized (..),
               Word16LE, Word32LE (..), byteSizeFoldable, getExact, getRemainingByteArray, getRemainingSeq,
               getRemainingString, getSeq, putByteArray, putByteString, putSeq, putWord8)
import qualified Data.ByteString.Short as BSS
import Data.Default (Default (..))
import Data.Primitive (sizeofByteArray)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Scrapti.Binary (QuietArray (..))
import Scrapti.Common (ConvertErr, KnownLabel (..), Label, SimpleMarker (..), UnparsedBody, bssInit, bssLast, countSize,
                       getChunkSizeLE, getExpectLabel, guardChunk, labelSize, padCount, putChunkSizeLE)
import Scrapti.Dsp (PcmContainer (..), PcmMeta (..))
import Scrapti.Riff (Chunk (..), ChunkLabel (..), KnownChunk (..), KnownListChunk (..), labelRiff, peekChunkLabel)

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

-- | A string NUL-padded to align to short width
newtype PaddedString = PaddedString { unPaddedString :: ShortByteString }
  deriving stock (Show)
  deriving newtype (Eq, IsString)

instance Default PaddedString where
  def = PaddedString BSS.empty

instance ByteSized PaddedString where
  byteSize (PaddedString sbs) = padCount (byteSize sbs)

mkPaddedString :: ShortByteString -> PaddedString
mkPaddedString sbs =
  PaddedString $ if not (BSS.null sbs) && bssLast sbs == 0
    then bssInit sbs
    else sbs

instance Binary PaddedString where
  get = do
    sbs <- getRemainingString
    pure $! mkPaddedString sbs
  put (PaddedString sbs) = do
    putByteString sbs
    let !usz = BSS.length sbs
    unless (even usz) (putWord8 0)

data WavFormatBody = WavFormatBody
  { wfbFormatType :: !Word16LE
  , wfbNumChannels :: !Word16LE
  , wfbSampleRate :: !Word32LE
  , wfbBitsPerSample :: !Word16LE
  , wfbExtra :: !ShortByteString
  } deriving stock (Eq, Show)

instance Default WavFormatBody where
  def = WavFormatBody 1 1 44100 16 mempty

instance ByteSized WavFormatBody where
  byteSize wf = 16 + byteSize (wfbExtra wf)

isSupportedBPS :: Word16LE -> Bool
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
    unless (bpsSlice == div bps 8 * numChannels) (fail ("Bad bps slice: " ++ show bpsSlice))
    unless (bpsAvg == sampleRate * fromIntegral bpsSlice) (fail ("Bad average bps: " ++ show bpsAvg))
    extra <- getRemainingString
    let !extraLen = byteSize extra
    unless (isSupportedFmtExtraSize extraLen) (fail ("Bad extra length: " ++ show extraLen))
    pure $! WavFormatBody formatType numChannels sampleRate bps extra
  put (WavFormatBody fty nchan sr bps extra) = do
    let !bpsSlice = div bps 8 * nchan
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

newtype WavDataBody = WavDataBody { unWavDataBody :: QuietArray }
  deriving stock (Show)
  deriving newtype (Eq, Default)

instance KnownLabel WavDataBody where
  knownLabel _ = labelData

instance ByteSized WavDataBody where
  byteSize (WavDataBody (QuietArray arr)) = fromIntegral (sizeofByteArray arr)

instance Binary WavDataBody where
  get = fmap (WavDataBody . QuietArray) getRemainingByteArray
  put (WavDataBody (QuietArray arr)) = putByteArray arr

type WavDataChunk = KnownChunk WavDataBody

data WavInfoElem = WavInfoElem
  { wieKey :: !Label
  , wieVal :: !PaddedString
  } deriving stock (Eq, Show)

instance ByteSized WavInfoElem where
  byteSize (WavInfoElem _ val) = labelSize + countSize + byteSize val

instance Binary WavInfoElem where
  get = do
    key <- get
    sz <- getChunkSizeLE
    val <- getExact sz get
    pure $! WavInfoElem key val
  put (WavInfoElem key val) = do
    put key
    putChunkSizeLE (byteSize val)
    put val

instance KnownLabel WavInfoElem where
  knownLabel _ = labelInfo

type WavInfoChunk = KnownListChunk WavInfoElem

-- NOTE: these are all the same for now, but ltxt has additional
-- structure that could be parsed out later
data WavAdtlData =
    WavAdtlDataLabl !PaddedString
  | WavAdtlDataNote !PaddedString
  | WavAdtlDataLtxt !PaddedString
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
    sz <- getChunkSizeLE
    (cueId, bs) <- getExact sz $ do
      cueId <- get
      bs <- get
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
    putChunkSizeLE (byteSize dat)
    put cueId
    case dat of
      WavAdtlDataLabl bs -> put bs
      WavAdtlDataNote bs -> put bs
      WavAdtlDataLtxt bs -> put bs

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

type WavUnparsedChunk = Chunk UnparsedBody

data WavChunk =
    WavChunkFormat !WavFormatChunk
  | WavChunkData !WavDataChunk
  | WavChunkInfo !WavInfoChunk
  | WavChunkAdtl !WavAdtlChunk
  | WavChunkCue !WavCueChunk
  | WavChunkUnparsed !WavUnparsedChunk
  deriving stock (Eq, Show)

instance ByteSized WavChunk where
  byteSize = \case
    WavChunkFormat x -> byteSize x
    WavChunkData x -> byteSize x
    WavChunkUnparsed x -> byteSize x
    WavChunkInfo x -> byteSize x
    WavChunkAdtl x -> byteSize x
    WavChunkCue x -> byteSize x

instance Binary WavChunk where
  get = do
    chunkLabel <- peekChunkLabel
    case chunkLabel of
      ChunkLabelSingle label | label == labelFmt -> fmap WavChunkFormat get
      ChunkLabelSingle label | label == labelData -> fmap WavChunkData get
      ChunkLabelList label | label == labelInfo -> fmap WavChunkInfo get
      ChunkLabelList label | label == labelAdtl -> fmap WavChunkAdtl get
      ChunkLabelSingle label | label == labelCue -> fmap WavChunkCue get
      _ -> fmap WavChunkUnparsed get
  put = \case
    WavChunkFormat x -> put x
    WavChunkData x -> put x
    WavChunkInfo x -> put x
    WavChunkAdtl x -> put x
    WavChunkCue x -> put x
    WavChunkUnparsed x -> put x

newtype WavHeader = WavHeader
  { wavHeaderRemainingSize :: ByteCount
  } deriving stock (Show)
    deriving newtype (Eq)
    deriving (ByteSized) via (ViaStaticByteSized WavHeader)

wavHeaderSize :: ByteCount
wavHeaderSize = 2 * labelSize + countSize

instance StaticByteSized WavHeader where
  staticByteSize _ = wavHeaderSize

instance Binary WavHeader where
  get = do
    getExpectLabel labelRiff
    sz <- getChunkSizeLE
    getExpectLabel labelWave
    pure $! WavHeader (sz - labelSize)
  put (WavHeader remSz) = do
    put labelRiff
    putChunkSizeLE (remSz + labelSize)
    put labelWave

newtype Wav = Wav
  { wavChunks :: Seq WavChunk
  } deriving stock (Eq, Show)

instance ByteSized Wav where
  byteSize (Wav chunks) = wavHeaderSize + byteSizeFoldable chunks

instance Binary Wav where
  get = do
    WavHeader remSz <- get
    chunks <- getExact remSz (getRemainingSeq get)
    pure $! Wav chunks
  put (Wav chunks)= do
    let !remSz = byteSizeFoldable chunks
    put (WavHeader remSz)
    putSeq put chunks

lookupWavChunk :: (WavChunk -> Bool) -> Wav -> Maybe WavChunk
lookupWavChunk p (Wav chunks) = fmap (Seq.index chunks) (Seq.findIndexL p chunks)

bindWavChunk :: (WavChunk -> Seq WavChunk) -> Wav -> Wav
bindWavChunk f (Wav chunks) = Wav (chunks >>= f)

lookupWavFormatChunk :: Wav -> Maybe WavFormatChunk
lookupWavFormatChunk w =
  case lookupWavChunk (\case { WavChunkFormat _ -> True; _ -> False }) w of
    Just (WavChunkFormat x) -> Just x
    _ -> Nothing

lookupWavDataChunk :: Wav -> Maybe WavDataChunk
lookupWavDataChunk w =
  case lookupWavChunk (\case { WavChunkData _ -> True; _ -> False }) w of
    Just (WavChunkData x) -> Just x
    _ -> Nothing

wavToPcmContainer :: Wav -> Either ConvertErr PcmContainer
wavToPcmContainer wav = do
  KnownChunk fmtBody <- guardChunk "format" (lookupWavFormatChunk wav)
  KnownChunk (WavDataBody (QuietArray arr)) <- guardChunk "data" (lookupWavDataChunk wav)
  let !nc = fromIntegral (wfbNumChannels fmtBody)
      !bps = fromIntegral (wfbBitsPerSample fmtBody)
      !sr = fromIntegral (wfbSampleRate fmtBody)
      !ns = div (sizeofByteArray arr) (nc * div bps 8)
      !meta = PcmMeta nc ns bps sr
  pure $! PcmContainer meta arr

wavFromPcmContainer :: PcmContainer -> Wav
wavFromPcmContainer (PcmContainer (PcmMeta {..}) arr) =
  let fmtBody = WavFormatBody
        { wfbFormatType = 1
        , wfbNumChannels = fromIntegral pmNumChannels
        , wfbSampleRate = fromIntegral pmSampleRate
        , wfbBitsPerSample = fromIntegral pmBitsPerSample
        , wfbExtra = mempty
        }
      fmtChunk = WavChunkFormat (KnownChunk fmtBody)
      dataChunk = WavChunkData (KnownChunk (WavDataBody (QuietArray arr)))
  in Wav (Seq.fromList [fmtChunk, dataChunk])

wcpFromMarker :: Int -> SimpleMarker -> WavCuePoint
wcpFromMarker ix sm = WavCuePoint (fromIntegral ix) (fromIntegral (smPosition sm)) 0 0 0 (fromIntegral (smPosition sm))

waeFromMarker :: Int -> SimpleMarker -> WavAdtlElem
waeFromMarker ix sm = WavAdtlElem (fromIntegral ix) (WavAdtlDataLabl (mkPaddedString (smName sm)))

wavUseMarkers :: Seq SimpleMarker -> (WavCueChunk, WavAdtlChunk)
wavUseMarkers marks =
  let wcps = Seq.mapWithIndex wcpFromMarker marks
      wcc = KnownChunk (WavCueBody wcps)
      waes = Seq.mapWithIndex waeFromMarker marks
      wac = KnownListChunk waes
  in (wcc, wac)

wavAddChunks :: Seq WavChunk -> Wav -> Wav
wavAddChunks chunks wav = wav { wavChunks = wavChunks wav <> chunks }
