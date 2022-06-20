{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Sfont where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word8)
import Scrapti.Binary (ByteLength, DecodeT, Get, decodeBounded, decodeGet, decodeRepeated, getByteString, getInt16le,
                       getVec, getWord16le, getWord32le, getWord8, skip)
import Scrapti.Riff (expectLabel, getLabel, labelRiff)
import Scrapti.Wav (WavData (..))

data Sfont = Sfont
  { sfontInfos :: !(Seq Info)
  , sfontSdta :: !Sdta
  , sfontPdta :: !Pdta
  } deriving stock (Eq, Show)

-- decodeSfont :: Monad m => DecodeT m Sfont
-- decodeSfont = do
--   expectLabel labelRiff

-- getSfontHeader :: Get ()
-- getSfontHeader = do
--   expectLabel labelRiff

-- data SfontChunk =
--     SfontChunkInfo !Info
--   | SfontChunkSdta !Sdta
--   | SfontChunkPdta !Pdta
--   deriving stock (Eq, Show)

-- getSfontChunk :: Get SfontChunk
-- getSfontChunk = do

labelSfbk, labelList, labelInfo, labelIfil, labelIver, labelIsng, labelInam, labelIrom, labelIcrd,
  labelIeng, labelIprd, labelIcop, labelIcmt, labelIsft, labelSdta, labelSmpl, labelSm24 :: ByteString
labelSfbk = "sfbk"
labelList = "LIST"
labelInfo = "INFO"
labelIfil = "ifil"
labelIver = "iver"
labelIsng = "isng"
labelInam = "INAM"
labelIrom = "irom"
labelIcrd = "ICRD"
labelIeng = "IENG"
labelIprd = "IPRD"
labelIcop = "ICOP"
labelIcmt = "ICMT"
labelIsft = "ISFT"
labelSdta = "sdta"
labelSmpl = "smpl"
labelSm24 = "sm24"

getSfontHeader :: Get ByteLength
getSfontHeader = do
  expectLabel labelRiff
  chunkSize <- getWord32le
  expectLabel labelSfbk
  pure $! fromIntegral chunkSize - 4

decodeSfont :: Monad m => DecodeT m Sfont
decodeSfont = do
  remainingSize <- decodeGet getSfontHeader
  decodeBounded remainingSize $ do
    infos <- decodeInfos
    sdta <- decodeGet getSdta
    pdta <- undefined
    pure $! Sfont infos sdta pdta

getInfosHeader :: Get ByteLength
getInfosHeader = do
  expectLabel labelList
  chunkSize <- getWord32le
  expectLabel labelInfo
  pure $! fromIntegral chunkSize - 4

getZstr :: Word32 -> Get Text
getZstr len = do
  bs <- getByteString (fromIntegral len)
  case BS.unsnoc bs of
    Nothing -> fail "empty zstr"
    Just (bs', nul) -> do
      unless (nul == 0) (fail "bad null byte")
      let !bs'' = if not (BS.null bs') && BS.last bs' == 0 then BS.init bs' else bs'
      pure $! TE.decodeLatin1 bs''

getInfo :: Get Info
getInfo = do
  label <- getLabel
  chunkSize <- getWord32le
  if
    | label == labelIfil -> do
      unless (chunkSize == 4) (fail "bad ifil chunk size")
      w1 <- getWord16le
      w2 <- getWord16le
      pure $! InfoVersion w1 w2
    | label == labelIver -> do
      unless (chunkSize == 4) (fail "bad iver chunk size")
      w1 <- getWord16le
      w2 <- getWord16le
      pure $! InfoRomVersion w1 w2
    | label == labelIsng ->
      InfoTargetSoundEngine <$> getZstr chunkSize
    | label == labelInam -> do
      InfoBankName <$> getZstr chunkSize
    | label == labelIrom -> do
      InfoRomName <$> getZstr chunkSize
    | label == labelIcrd -> do
      InfoCreationDate <$> getZstr chunkSize
    | label == labelIeng -> do
      InfoAuthors <$> getZstr chunkSize
    | label == labelIprd -> do
      InfoIntendedProduct <$> getZstr chunkSize
    | label == labelIcop -> do
      InfoCopyrightMessage <$> getZstr chunkSize
    | label == labelIcmt -> do
      InfoComments <$> getZstr chunkSize
    | label == labelIsft -> do
      InfoUsedTools <$> getZstr chunkSize
    | otherwise -> do
      bs <- getByteString (fromIntegral chunkSize)
      pure $! InfoReserved label bs

decodeInfos :: Monad m => DecodeT m (Seq Info)
decodeInfos = do
  remainingSize <- decodeGet getInfosHeader
  decodeRepeated remainingSize (decodeGet getInfo)


-- getLowBits :: Get (WavData Int16)
-- getLowBits bitsPer getter = do
--   let !bytesPer = div bitsPer 8
--   chunkSize <- getWord32le
--   unless (mod chunkSize (fromIntegral bytesPer) == 0) (fail "bad data chunk size")
--   let !samples = fromIntegral (div chunkSize (fromIntegral bytesPer))
--   vec <- getVec samples getter
--   unless (VU.length vec == samples) (fail "bad samples")
--   pure $! WavData vec

getHighBits :: Word32 -> Get (WavData Int16)
getHighBits numSamples = WavData <$> getVec (fromIntegral numSamples) getInt16le

getLowBits :: Word32 -> Get (WavData Word8)
getLowBits numSamples = WavData <$> getVec (fromIntegral numSamples) getWord8

getSdta :: Get Sdta
getSdta = do
  expectLabel labelList
  chunkSize <- getWord32le
  expectLabel labelSdta
  expectLabel labelSmpl
  highSize <- getWord32le
  let !numSamples = div highSize 2
  highBits <- getHighBits numSamples
  let !numExtra = chunkSize - highSize - 12
  if
    | numExtra > 0 -> do
      expectLabel labelSm24
      lowSize <- getWord32le
      let !expectedSize = if even numSamples then numSamples else numSamples + 1
      unless (lowSize == expectedSize) (fail "invalid low sample size")
      lowBits <- getLowBits numSamples
      unless (even numSamples) (skip 1)
      pure $! Sdta highBits (Just lowBits)
    | numExtra == 0 -> pure $! Sdta highBits Nothing
    | otherwise -> fail "invalid sdata chunk/sample sizes"

data Info =
    InfoVersion !Word16 !Word16
  | InfoTargetSoundEngine !Text
  | InfoBankName !Text
  | InfoRomName !Text
  | InfoRomVersion !Word16 !Word16
  | InfoCreationDate !Text
  | InfoAuthors !Text
  | InfoIntendedProduct !Text
  | InfoCopyrightMessage !Text
  | InfoComments !Text
  | InfoUsedTools !Text
  | InfoReserved !ByteString !ByteString
  deriving stock (Eq, Show)

data Sdta = Sdta
  { sdtaHighBits :: !(WavData Int16)
  , sdtaLowBits :: !(Maybe (WavData Word8))
  } deriving stock (Eq, Show)

data Pdta = Pdta
  { pdtaPhdrs :: !(Seq Phdr)
  -- ^ Preset headers
  , pdtaPbags :: !(Seq Bag)
  -- ^ Preset bags
  , pdtaPmods :: !(Seq Mod)
  -- ^ Preset modulators
  , pdtaPgens :: !(Seq Gen)
  -- ^ Preset generators
  , pdtaInsts :: !(Seq Inst)
  -- ^ Instrument names
  , pdtaIbags :: !(Seq Bag)
  -- ^ Instrument bags
  , pdtaImods :: !(Seq Mod)
  -- ^ Instrument modulators
  , pdtaIgens :: !(Seq Gen)
  -- ^ Instrument generators
  , pdtaShdrs :: !(Seq Shdr)
  -- ^ Sample headers
  } deriving stock (Eq, Show)

-- | Preset header
data Phdr = Phdr
  { phdrPresetName :: !Text
  , phdrPreset :: !Word16
  , phdrBank :: !Word16
  , phdrPresetBagIndex :: !Word16
  , phdrLibrary :: !Word32
  , phdrGenre :: !Word32
  , phdrMorphology :: !Word32
  } deriving stock (Eq, Show)

data Bag = Bag
  { bagGenIndex :: !Word16
  , bagModIndex :: !Word16
  } deriving stock (Eq, Show)

-- | Modulator
data Mod = Mod
  { modSrcOper :: !Word16
  , modDestOper :: !Word16
  , modAmount :: !Int16
  , modAmtSrcOper :: !Word16
  , modTransOper :: !Word16
  } deriving stock (Eq, Show)

data SampleMode =
    SampleModeNoLoop
  | SampleModeContLoop
  | SampleModePressLoop
  deriving stock (Eq, Show)

-- | Generator
data Gen =
    GenStartAddressOffset !Int
  | GenEndAddressOffset !Int
  | GenLoopStartAddressOffset !Int
  | GenLoopEndAddressOffset !Int
  | GenStartAddressCoarseOffset !Int
  | GenModLfoToPitch !Int
  | GenVibLfoToPitch !Int
  | GenModEnvToPitch !Int
  | GenInitFc !Int
  | GenInitQ !Int
  | GenModLfoToFc !Int
  | GenModEnvToFc !Int
  | GenEndAddressCoarseOffset !Int
  | GenModLfoToVol !Int
  | GenChorus !Int
  | GenReverb !Int
  | GenPan !Int
  | GenDelayModLfo !Int
  | GenFreqModLfo !Int
  | GenDelayVibLfo !Int
  | GenFreqVibLfo !Int
  | GenDelayModEnv !Int
  | GenAttackModEnv !Int
  | GenHoldModEnv !Int
  | GenDecayModEnv !Int
  | GenSustainModEnv !Int
  | GenReleaseModEnv !Int
  | GenKeyToModEnvHold !Int
  | GenKeyToModEnvDecay !Int
  | GenDelayVolEnv !Int
  | GenAttackVolEnv !Int
  | GenHoldVolEnv !Int
  | GenDecayVolEnv !Int
  | GenSustainVolEnv !Int
  | GenReleaseVolEnv !Int
  | GenKeyToVolEnvHold !Int
  | GenKeyToVolEnvDecay !Int
  | GenInstIndex !Word
  | GenKeyRange !Word !Word
  | GenVelRange !Word !Word
  | GenLoopStartAddressCoarseOffset !Int
  | GenKey !Word
  | GenVel !Word
  | GenInitAtten !Int
  | GenLoopEndAddressCoarseOffset !Int
  | GenCoarseTune !Int
  | GenFineTune !Int
  | GenSampleIndex !Word
  | GenSampleMode !SampleMode
  | GenScaleTuning !Int
  | GenExclusiveClass !Int
  | GenRootKey !Word
  | GenReserved !Int !Int
  deriving stock (Eq, Show)

-- | Instrument
data Inst = Inst
  { instName :: !Text
  , instBagIndex :: !Word16
  } deriving stock (Eq, Show)

-- | Sample header
data Shdr = Shdr
  { shdrSampleName :: !Text
  , shdrStart :: !Word
  , shdrEnd :: !Word
  , shdrStartLoop :: !Word
  , shdrEndLoop :: !Word
  , shdrSampleRate :: !Word
  , shdrOriginalPitch :: !Word
  , shdrPitchCorrection :: !Int
  , shdrSampleLink :: !Word
  , shdrSampleType :: !Word
  } deriving stock (Eq, Show)
