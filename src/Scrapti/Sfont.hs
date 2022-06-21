{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Sfont where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import Data.Int (Int16, Int8)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word8)
import Scrapti.Binary (ByteLength, DecodeT, Get, decodeBounded, decodeGet, decodeMonoid, decodeRepeated, getByteString,
                       getInt16le, getInt8, getSeq, getVec, getWord16le, getWord32le, getWord8, skip)
import Scrapti.Riff (expectLabel, getLabel, labelRiff)
import Scrapti.Wav (WavData (..))

data Sfont = Sfont
  { sfontInfos :: !(Seq Info)
  , sfontSdta :: !Sdta
  , sfontPdta :: !(Seq PdtaElem)
  } deriving stock (Eq, Show)

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

data PdtaCat =
    PdataCatPreset
  | PdataCatInst
  deriving stock (Eq, Show)

data PdtaElem =
    PdtaElemPhdr !Phdr
  | PdtaElemBag !PdtaCat !Bag
  | PdtaElemMod !PdtaCat !Mod
  | PdtaElemGen !PdtaCat !Gen
  | PdtaElemInst !Inst
  | PdtaElemShdr !Shdr
  deriving stock (Eq, Show)

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

emptyPdta :: Pdta
emptyPdta = Pdta Empty Empty Empty Empty Empty Empty Empty Empty Empty

buildPdta :: Seq PdtaElem -> Pdta
buildPdta = foldl' go emptyPdta where
  go p = \case
    PdtaElemPhdr phdr -> p { pdtaPhdrs = pdtaPhdrs p :|> phdr }
    PdtaElemBag cat bag -> case cat of
      PdataCatPreset -> p { pdtaPbags = pdtaPbags p :|> bag }
      PdataCatInst -> p { pdtaIbags = pdtaIbags p :|> bag }
    PdtaElemMod cat modd -> case cat of
      PdataCatPreset -> p { pdtaPmods = pdtaPmods p :|> modd }
      PdataCatInst -> p { pdtaImods = pdtaImods p :|> modd }
    PdtaElemGen cat gen -> case cat of
      PdataCatPreset -> p { pdtaPgens = pdtaPgens p :|> gen }
      PdataCatInst -> p { pdtaIgens = pdtaIgens p :|> gen }
    PdtaElemInst inst -> p { pdtaInsts = pdtaInsts p :|> inst }
    PdtaElemShdr shdr -> p { pdtaShdrs = pdtaShdrs p :|> shdr }

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
    GenStartAddressOffset !Int16
  | GenEndAddressOffset !Int16
  | GenLoopStartAddressOffset !Int16
  | GenLoopEndAddressOffset !Int16
  | GenStartAddressCoarseOffset !Int16
  | GenModLfoToPitch !Int16
  | GenVibLfoToPitch !Int16
  | GenModEnvToPitch !Int16
  | GenInitFc !Int16
  | GenInitQ !Int16
  | GenModLfoToFc !Int16
  | GenModEnvToFc !Int16
  | GenEndAddressCoarseOffset !Int16
  | GenModLfoToVol !Int16
  | GenChorus !Int16
  | GenReverb !Int16
  | GenPan !Int16
  | GenDelayModLfo !Int16
  | GenFreqModLfo !Int16
  | GenDelayVibLfo !Int16
  | GenFreqVibLfo !Int16
  | GenDelayModEnv !Int16
  | GenAttackModEnv !Int16
  | GenHoldModEnv !Int16
  | GenDecayModEnv !Int16
  | GenSustainModEnv !Int16
  | GenReleaseModEnv !Int16
  | GenKeyToModEnvHold !Int16
  | GenKeyToModEnvDecay !Int16
  | GenDelayVolEnv !Int16
  | GenAttackVolEnv !Int16
  | GenHoldVolEnv !Int16
  | GenDecayVolEnv !Int16
  | GenSustainVolEnv !Int16
  | GenReleaseVolEnv !Int16
  | GenKeyToVolEnvHold !Int16
  | GenKeyToVolEnvDecay !Int16
  | GenInstIndex !Word16
  | GenKeyRange !Word8 !Word8
  | GenVelRange !Word8 !Word8
  | GenLoopStartAddressCoarseOffset !Int16
  | GenKey !Word16
  | GenVel !Word16
  | GenInitAtten !Int16
  | GenLoopEndAddressCoarseOffset !Int16
  | GenCoarseTune !Int16
  | GenFineTune !Int16
  | GenSampleIndex !Word16
  | GenSampleMode !SampleMode
  | GenScaleTuning !Int16
  | GenExclusiveClass !Int16
  | GenRootKey !Word16
  | GenReserved !Int16 !Int16
  deriving stock (Eq, Show)

-- | Instrument
data Inst = Inst
  { instName :: !Text
  , instBagIndex :: !Word16
  } deriving stock (Eq, Show)

-- | Sample header
data Shdr = Shdr
  { shdrSampleName :: !Text
  , shdrStart :: !Word32
  , shdrEnd :: !Word32
  , shdrStartLoop :: !Word32
  , shdrEndLoop :: !Word32
  , shdrSampleRate :: !Word32
  , shdrOriginalPitch :: !Word8
  , shdrPitchCorrection :: !Int8
  , shdrSampleLink :: !Word16
  , shdrSampleType :: !Word16
  } deriving stock (Eq, Show)

labelSfbk, labelList, labelInfo, labelIfil, labelIver, labelIsng, labelInam, labelIrom, labelIcrd,
  labelIeng, labelIprd, labelIcop, labelIcmt, labelIsft, labelSdta, labelSmpl, labelSm24,
  labelPdta, labelPhdr, labelPbag, labelPmod, labelPgen, labelInst, labelIbag, labelImod, labelIgen, labelShdr :: ByteString
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
labelPdta = "pdta"
labelPhdr = "phdr"
labelPbag = "pbag"
labelPmod = "pmod"
labelPgen = "pgen"
labelInst = "inst"
labelIbag = "ibag"
labelImod = "imod"
labelIgen = "igen"
labelShdr = "shdr"

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
    pdta <- decodePdta
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
    | label == labelIsng -> InfoTargetSoundEngine <$> getZstr chunkSize
    | label == labelInam -> InfoBankName <$> getZstr chunkSize
    | label == labelIrom -> InfoRomName <$> getZstr chunkSize
    | label == labelIcrd -> InfoCreationDate <$> getZstr chunkSize
    | label == labelIeng -> InfoAuthors <$> getZstr chunkSize
    | label == labelIprd -> InfoIntendedProduct <$> getZstr chunkSize
    | label == labelIcop -> InfoCopyrightMessage <$> getZstr chunkSize
    | label == labelIcmt -> InfoComments <$> getZstr chunkSize
    | label == labelIsft -> InfoUsedTools <$> getZstr chunkSize
    | otherwise -> do
      bs <- getByteString (fromIntegral chunkSize)
      pure $! InfoReserved label bs

decodeInfos :: Monad m => DecodeT m (Seq Info)
decodeInfos = do
  remainingSize <- decodeGet getInfosHeader
  decodeRepeated remainingSize (decodeGet getInfo)

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

getPdtaHeader :: Get ByteLength
getPdtaHeader = do
  expectLabel labelList
  pdtaSize <- getWord32le
  expectLabel labelPdta
  pure $! fromIntegral pdtaSize - 4

sizePhdr, sizeBag, sizeMod, sizeGen, sizeInst, sizeShdr :: ByteLength
sizePhdr = 38
sizeBag = 4
sizeMod = 10
sizeGen = 4
sizeInst = 22
sizeShdr = 46

getPdtaElemChunk :: ByteString -> ByteLength -> ByteLength -> Get PdtaElem -> Get (Seq PdtaElem)
getPdtaElemChunk label chunkLen size getter = do
  unless (mod chunkLen size == 0) (fail ("invalid size for pdta elem: " ++ show label))
  let !numElems = div chunkLen sizePhdr
  getSeq (fromIntegral numElems) getter

getPhdr :: Get Phdr
getPhdr = do
  rawPresetName <- getByteString 20
  let !presetName = TE.decodeLatin1 (BS.takeWhile (/= 0) rawPresetName)
  preset <- getWord16le
  bank <- getWord16le
  presetBagIndex <- getWord16le
  library <- getWord32le
  genre <- getWord32le
  morphology <- getWord32le
  pure $! Phdr
    { phdrPresetName = presetName
    , phdrPreset = preset
    , phdrBank = bank
    , phdrPresetBagIndex = presetBagIndex
    , phdrLibrary = library
    , phdrGenre = genre
    , phdrMorphology = morphology
    }

getBag :: Get Bag
getBag = do
  genIndex <- getWord16le
  modIndex <- getWord16le
  pure $! Bag
    { bagGenIndex = genIndex
    , bagModIndex = modIndex
    }

getMod :: Get Mod
getMod = do
  srcOper <- getWord16le
  destOper <- getWord16le
  amount <- getInt16le
  amtSrcOper <- getWord16le
  transOper <- getWord16le
  pure $! Mod
    { modSrcOper = srcOper
    , modDestOper = destOper
    , modAmount = amount
    , modAmtSrcOper = amtSrcOper
    , modTransOper = transOper
    }

getGen :: Get Gen
getGen = do
  tag <- getInt16le
  if
    | tag == 0 -> fmap GenStartAddressOffset getInt16le
    | tag == 1 -> fmap GenEndAddressOffset getInt16le
    | tag == 2 -> fmap GenLoopStartAddressOffset getInt16le
    | tag == 3 -> fmap GenLoopEndAddressOffset getInt16le
    | tag == 4 -> fmap GenStartAddressCoarseOffset getInt16le
    | tag == 5 -> fmap GenModLfoToPitch getInt16le
    | tag == 6 -> fmap GenVibLfoToPitch getInt16le
    | tag == 7 -> fmap GenModEnvToPitch getInt16le
    | tag == 8 -> fmap GenInitFc getInt16le
    | tag == 9 -> fmap GenInitQ getInt16le
    | tag == 10 -> fmap GenModLfoToFc getInt16le
    | tag == 11 -> fmap GenModEnvToFc getInt16le
    | tag == 12 -> fmap GenEndAddressCoarseOffset getInt16le
    | tag == 13 -> fmap GenModLfoToVol getInt16le
    | tag == 15 -> fmap GenChorus getInt16le
    | tag == 16 -> fmap GenReverb getInt16le
    | tag == 17 -> fmap GenPan getInt16le
    | tag == 21 -> fmap GenDelayModLfo getInt16le
    | tag == 22 -> fmap GenFreqModLfo getInt16le
    | tag == 23 -> fmap GenDelayVibLfo getInt16le
    | tag == 24 -> fmap GenFreqVibLfo getInt16le
    | tag == 25 -> fmap GenDelayModEnv getInt16le
    | tag == 26 -> fmap GenAttackModEnv getInt16le
    | tag == 27 -> fmap GenHoldModEnv getInt16le
    | tag == 28 -> fmap GenDecayModEnv getInt16le
    | tag == 29 -> fmap GenSustainModEnv getInt16le
    | tag == 30 -> fmap GenReleaseModEnv getInt16le
    | tag == 31 -> fmap GenKeyToModEnvHold getInt16le
    | tag == 32 -> fmap GenKeyToModEnvDecay getInt16le
    | tag == 33 -> fmap GenDelayVolEnv getInt16le
    | tag == 34 -> fmap GenAttackVolEnv getInt16le
    | tag == 35 -> fmap GenHoldVolEnv getInt16le
    | tag == 36 -> fmap GenDecayVolEnv getInt16le
    | tag == 37 -> fmap GenSustainVolEnv getInt16le
    | tag == 38 -> fmap GenReleaseVolEnv getInt16le
    | tag == 39 -> fmap GenKeyToVolEnvHold getInt16le
    | tag == 40 -> fmap GenKeyToVolEnvDecay getInt16le
    | tag == 41 -> fmap GenInstIndex getWord16le
    | tag == 43 -> do
      a <- getWord8
      b <- getWord8
      pure $! GenKeyRange a b
    | tag == 44 -> do
      a <- getWord8
      b <- getWord8
      pure $! GenVelRange a b
    | tag == 45 -> fmap GenLoopStartAddressCoarseOffset getInt16le
    | tag == 46 -> fmap GenKey getWord16le
    | tag == 47 -> fmap GenVel getWord16le
    | tag == 48 -> fmap GenInitAtten getInt16le
    | tag == 50 -> fmap GenLoopEndAddressCoarseOffset getInt16le
    | tag == 51 -> fmap GenCoarseTune getInt16le
    | tag == 52 -> fmap GenFineTune getInt16le
    | tag == 53 -> fmap GenSampleIndex getWord16le
    | tag == 54 -> do
      a <- getInt16le
      let !sm = case a of
            1 -> SampleModeContLoop
            3 -> SampleModePressLoop
            _ -> SampleModeNoLoop
      pure $! GenSampleMode sm
    | tag == 56 -> fmap GenScaleTuning getInt16le
    | tag == 57 -> fmap GenExclusiveClass getInt16le
    | tag == 58 -> fmap GenRootKey getWord16le
    | otherwise -> do
      a <- getInt16le
      pure $! GenReserved tag a

getInst :: Get Inst
getInst = do
  rawName <- getByteString 20
  let !name = TE.decodeLatin1 (BS.takeWhile (/= 0) rawName)
  bagIndex <- getWord16le
  pure $! Inst
    { instName = name
    , instBagIndex = bagIndex
    }

getShdr :: Get Shdr
getShdr = do
  rawName <- getByteString 20
  let !name = TE.decodeLatin1 (BS.takeWhile (/= 0) rawName)
  start <- getWord32le
  end <- getWord32le
  startLoop <- getWord32le
  endLoop <- getWord32le
  sampleRate <- getWord32le
  originalPitch <- getWord8
  pitchCorrection <- getInt8
  sampleLink <- getWord16le
  sampleType <- getWord16le
  pure $! Shdr
    { shdrSampleName = name
    , shdrStart = start
    , shdrEnd = end
    , shdrStartLoop = startLoop
    , shdrEndLoop = endLoop
    , shdrSampleRate = sampleRate
    , shdrOriginalPitch = originalPitch
    , shdrPitchCorrection = pitchCorrection
    , shdrSampleLink = sampleLink
    , shdrSampleType = sampleType
    }

getPdtaElem :: Get (Seq PdtaElem)
getPdtaElem = do
  label <- getLabel
  chunkSize <- getWord32le
  let !chunkLen = fromIntegral chunkSize
      get = getPdtaElemChunk label chunkLen
  if
    | label == labelPhdr ->
      get sizePhdr (fmap PdtaElemPhdr getPhdr)
    | label == labelPbag ->
      get sizeBag (fmap (PdtaElemBag PdataCatPreset) getBag)
    | label == labelPmod ->
      get sizeMod (fmap (PdtaElemMod PdataCatPreset) getMod)
    | label == labelPgen ->
      get sizeGen (fmap (PdtaElemGen PdataCatPreset) getGen)
    | label == labelInst ->
      get sizeInst (fmap PdtaElemInst getInst)
    | label == labelIbag ->
      get sizeBag (fmap (PdtaElemBag PdataCatInst) getBag)
    | label == labelImod ->
      get sizeMod (fmap (PdtaElemMod PdataCatInst) getMod)
    | label == labelIgen ->
      get sizeGen (fmap (PdtaElemGen PdataCatInst) getGen)
    | label == labelShdr ->
      get sizeInst (fmap PdtaElemShdr getShdr)
    | otherwise ->
      fail ("unrecognized pdta elem: " ++ show label)

decodePdta :: Monad m => DecodeT m (Seq PdtaElem)
decodePdta = do
  remainingBytes <- decodeGet getPdtaHeader
  decodeMonoid remainingBytes (decodeGet getPdtaElem)
