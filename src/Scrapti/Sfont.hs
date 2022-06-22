{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Sfont
  ( Sfont (..)
  , Info (..)
  , Sdta (..)
  , PdtaCat (..)
  , PdtaBlock (..)
  , Pdta (..)
  , Phdr (..)
  , Bag (..)
  , Mod (..)
  , Gen (..)
  , Inst (..)
  , Shdr (..)
  , buildPdta
  , decodeSfont
  , encodeSfont
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (foldMap', foldl')
import Data.Int (Int16, Int8)
import Data.Semigroup (Sum (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word8)
import Scrapti.Binary (ByteLength, DecodeT, Get, Put, decodeBounded, decodeGet, decodeRepeated, getByteString,
                       getFixedString, getInt16le, getInt8, getSeq, getVec, getWord16le, getWord32le, getWord8,
                       guardEnd, putByteString, putFixedString, putInt16le, putInt8, putSeq, putVec, putWord16le,
                       putWord32le, putWord8, runPut, skip)
import Scrapti.Riff (expectLabel, getLabel, labelRiff)
import Scrapti.Wav (WavData (..), wavDataSamples)

data Sfont = Sfont
  { sfontInfos :: !(Seq Info)
  , sfontSdta :: !Sdta
  , sfontPdta :: !(Seq PdtaBlock)
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
    PdtaCatPreset
  | PdtaCatInst
  deriving stock (Eq, Show)

data PdtaBlock =
    PdtaBlockPhdr !(Seq Phdr)
  | PdtaBlockBag !PdtaCat !(Seq Bag)
  | PdtaBlockMod !PdtaCat !(Seq Mod)
  | PdtaBlockGen !PdtaCat !(Seq Gen)
  | PdtaBlockInst !(Seq Inst)
  | PdtaBlockShdr !(Seq Shdr)
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
    SampleModeNoLoop !Int16
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

emptyPdta :: Pdta
emptyPdta = Pdta Empty Empty Empty Empty Empty Empty Empty Empty Empty

buildPdta :: Seq PdtaBlock -> Pdta
buildPdta = foldl' go emptyPdta where
  go p = \case
    PdtaBlockPhdr phdrs -> p { pdtaPhdrs = pdtaPhdrs p <> phdrs }
    PdtaBlockBag cat bags -> case cat of
      PdtaCatPreset -> p { pdtaPbags = pdtaPbags p <> bags }
      PdtaCatInst -> p { pdtaIbags = pdtaIbags p <> bags }
    PdtaBlockMod cat mods -> case cat of
      PdtaCatPreset -> p { pdtaPmods = pdtaPmods p <> mods }
      PdtaCatInst -> p { pdtaImods = pdtaImods p <> mods }
    PdtaBlockGen cat gens -> case cat of
      PdtaCatPreset -> p { pdtaPgens = pdtaPgens p <> gens }
      PdtaCatInst -> p { pdtaIgens = pdtaIgens p <> gens }
    PdtaBlockInst insts -> p { pdtaInsts = pdtaInsts p <> insts }
    PdtaBlockShdr shdrs -> p { pdtaShdrs = pdtaShdrs p <> shdrs }

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
  sfont <- decodeBounded remainingSize $ do
    infos <- decodeInfos
    sdta <- decodeGet getSdta
    pdtaBlocks <- decodePdtaBlocks
    pure $! Sfont infos sdta pdtaBlocks
  guardEnd
  pure sfont

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

sizeZstr :: Text -> ByteLength
sizeZstr t = let len = fromIntegral (T.length t + 1) in if even len then len else len + 1

putZstr :: Text -> Put
putZstr t =
  let !bs0 = BSC.pack (T.unpack t)
      !bs1 = BS.snoc bs0 0
      !bs2 = if odd (BS.length bs1) then BS.snoc bs1 0 else bs1
  in putByteString bs2

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
  chunkSize <- getWord32le
  expectLabel labelPdta
  pure $! fromIntegral chunkSize - 4

sizePhdr, sizeBag, sizeMod, sizeGen, sizeInst, sizeShdr :: ByteLength
sizePhdr = 38
sizeBag = 4
sizeMod = 10
sizeGen = 4
sizeInst = 22
sizeShdr = 46

getPdtaElems :: ByteString -> ByteLength -> ByteLength -> Get a -> Get (Seq a)
getPdtaElems label chunkLen size getter = do
  unless (mod chunkLen size == 0) (fail ("invalid size for pdta elem: " ++ show label))
  let !numElems = div chunkLen size
  getSeq (fromIntegral numElems) getter

getShortString :: Get Text
getShortString = getFixedString 20

putShortString :: Text -> Put
putShortString = putFixedString 20

getPhdr :: Get Phdr
getPhdr = do
  presetName <- getShortString
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

putPhdr :: Phdr -> Put
putPhdr phdr = do
  putShortString (phdrPresetName phdr)
  putWord16le (phdrPreset phdr)
  putWord16le (phdrBank phdr)
  putWord16le (phdrPresetBagIndex phdr)
  putWord32le (phdrLibrary phdr)
  putWord32le (phdrGenre phdr)
  putWord32le (phdrMorphology phdr)

getBag :: Get Bag
getBag = do
  genIndex <- getWord16le
  modIndex <- getWord16le
  pure $! Bag
    { bagGenIndex = genIndex
    , bagModIndex = modIndex
    }

putBag :: Bag -> Put
putBag bag = do
  putWord16le (bagGenIndex bag)
  putWord16le (bagModIndex bag)

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

putMod :: Mod -> Put
putMod modd = do
  putWord16le (modSrcOper modd)
  putWord16le (modDestOper modd)
  putInt16le (modAmount modd)
  putWord16le (modAmtSrcOper modd)
  putWord16le (modTransOper modd)

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
            _ -> SampleModeNoLoop a
      pure $! GenSampleMode sm
    | tag == 56 -> fmap GenScaleTuning getInt16le
    | tag == 57 -> fmap GenExclusiveClass getInt16le
    | tag == 58 -> fmap GenRootKey getWord16le
    | otherwise -> do
      a <- getInt16le
      pure $! GenReserved tag a

whichTagGen :: Gen -> Int16
whichTagGen = \case
  GenStartAddressOffset _ -> 0
  GenEndAddressOffset _ -> 1
  GenLoopStartAddressOffset _ -> 2
  GenLoopEndAddressOffset _ -> 3
  GenStartAddressCoarseOffset _ -> 4
  GenModLfoToPitch _ -> 5
  GenVibLfoToPitch _ -> 6
  GenModEnvToPitch _ -> 7
  GenInitFc _ -> 8
  GenInitQ _ -> 9
  GenModLfoToFc _ -> 10
  GenModEnvToFc _ -> 11
  GenEndAddressCoarseOffset _ -> 12
  GenModLfoToVol _ -> 13
  GenChorus _ -> 15
  GenReverb _ -> 16
  GenPan _ -> 17
  GenDelayModLfo _ -> 21
  GenFreqModLfo _ -> 22
  GenDelayVibLfo _ -> 23
  GenFreqVibLfo _ -> 24
  GenDelayModEnv _ -> 25
  GenAttackModEnv _ -> 26
  GenHoldModEnv _ -> 27
  GenDecayModEnv _ -> 28
  GenSustainModEnv _ -> 29
  GenReleaseModEnv _ -> 30
  GenKeyToModEnvHold _ -> 31
  GenKeyToModEnvDecay _ -> 32
  GenDelayVolEnv _ -> 33
  GenAttackVolEnv _ -> 34
  GenHoldVolEnv _ -> 35
  GenDecayVolEnv _ -> 36
  GenSustainVolEnv _ -> 37
  GenReleaseVolEnv _ -> 38
  GenKeyToVolEnvHold _ -> 39
  GenKeyToVolEnvDecay _ -> 40
  GenInstIndex _ -> 41
  GenKeyRange _ _ -> 43
  GenVelRange _ _ -> 44
  GenLoopStartAddressCoarseOffset _ -> 45
  GenKey _ -> 46
  GenVel _ -> 47
  GenInitAtten _ -> 48
  GenLoopEndAddressCoarseOffset _ -> 50
  GenCoarseTune _ -> 51
  GenFineTune _ -> 52
  GenSampleIndex _ -> 53
  GenSampleMode _ -> 54
  GenScaleTuning _ -> 56
  GenExclusiveClass _ -> 57
  GenRootKey _ -> 58
  GenReserved t _ -> t

putGen :: Gen -> Put
putGen gen = do
  putInt16le (whichTagGen gen)
  case gen of
    GenStartAddressOffset x -> putInt16le x
    GenEndAddressOffset x -> putInt16le x
    GenLoopStartAddressOffset x -> putInt16le x
    GenLoopEndAddressOffset x -> putInt16le x
    GenStartAddressCoarseOffset x -> putInt16le x
    GenModLfoToPitch x -> putInt16le x
    GenVibLfoToPitch x -> putInt16le x
    GenModEnvToPitch x -> putInt16le x
    GenInitFc x -> putInt16le x
    GenInitQ x -> putInt16le x
    GenModLfoToFc x -> putInt16le x
    GenModEnvToFc x -> putInt16le x
    GenEndAddressCoarseOffset x -> putInt16le x
    GenModLfoToVol x -> putInt16le x
    GenChorus x -> putInt16le x
    GenReverb x -> putInt16le x
    GenPan x -> putInt16le x
    GenDelayModLfo x -> putInt16le x
    GenFreqModLfo x -> putInt16le x
    GenDelayVibLfo x -> putInt16le x
    GenFreqVibLfo x -> putInt16le x
    GenDelayModEnv x -> putInt16le x
    GenAttackModEnv x -> putInt16le x
    GenHoldModEnv x -> putInt16le x
    GenDecayModEnv x -> putInt16le x
    GenSustainModEnv x -> putInt16le x
    GenReleaseModEnv x -> putInt16le x
    GenKeyToModEnvHold x -> putInt16le x
    GenKeyToModEnvDecay x -> putInt16le x
    GenDelayVolEnv x -> putInt16le x
    GenAttackVolEnv x -> putInt16le x
    GenHoldVolEnv x -> putInt16le x
    GenDecayVolEnv x -> putInt16le x
    GenSustainVolEnv x -> putInt16le x
    GenReleaseVolEnv x -> putInt16le x
    GenKeyToVolEnvHold x -> putInt16le x
    GenKeyToVolEnvDecay x -> putInt16le x
    GenInstIndex x -> putWord16le x
    GenKeyRange x y -> putWord8 x *> putWord8 y
    GenVelRange x y -> putWord8 x *> putWord8 y
    GenLoopStartAddressCoarseOffset x -> putInt16le x
    GenKey x -> putWord16le x
    GenVel x -> putWord16le x
    GenInitAtten x -> putInt16le x
    GenLoopEndAddressCoarseOffset x -> putInt16le x
    GenCoarseTune x -> putInt16le x
    GenFineTune x -> putInt16le x
    GenSampleIndex x -> putWord16le x
    GenSampleMode sm ->
      let !x = case sm of
            SampleModeContLoop -> 1
            SampleModePressLoop -> 3
            SampleModeNoLoop c -> c
      in putInt16le x
    GenScaleTuning x -> putInt16le x
    GenExclusiveClass x -> putInt16le x
    GenRootKey x -> putWord16le x
    GenReserved _ x -> putInt16le x

getInst :: Get Inst
getInst = do
  name <- getShortString
  bagIndex <- getWord16le
  pure $! Inst
    { instName = name
    , instBagIndex = bagIndex
    }

putInst :: Inst -> Put
putInst inst = do
  putShortString (instName inst)
  putWord16le (instBagIndex inst)

getShdr :: Get Shdr
getShdr = do
  name <- getShortString
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

putShdr :: Shdr -> Put
putShdr shdr = do
  putShortString (shdrSampleName shdr)
  putWord32le (shdrStart shdr)
  putWord32le (shdrEnd shdr)
  putWord32le (shdrStartLoop shdr)
  putWord32le (shdrEndLoop shdr)
  putWord32le (shdrSampleRate shdr)
  putWord8 (shdrOriginalPitch shdr)
  putInt8 (shdrPitchCorrection shdr)
  putWord16le (shdrSampleLink shdr)
  putWord16le (shdrSampleType shdr)

getPdtaBlock :: Get PdtaBlock
getPdtaBlock = do
  label <- getLabel
  chunkSize <- getWord32le
  let !chunkLen = fromIntegral chunkSize
  if
    | label == labelPhdr ->
      fmap PdtaBlockPhdr (getPdtaElems label chunkLen sizePhdr getPhdr)
    | label == labelPbag ->
      fmap (PdtaBlockBag PdtaCatPreset) (getPdtaElems label chunkLen sizeBag getBag)
    | label == labelPmod ->
      fmap (PdtaBlockMod PdtaCatPreset) (getPdtaElems label chunkLen sizeMod getMod)
    | label == labelPgen ->
      fmap (PdtaBlockGen PdtaCatPreset) (getPdtaElems label chunkLen sizeGen getGen)
    | label == labelInst ->
      fmap PdtaBlockInst (getPdtaElems label chunkLen sizeInst getInst)
    | label == labelIbag ->
      fmap (PdtaBlockBag PdtaCatInst) (getPdtaElems label chunkLen sizeBag getBag)
    | label == labelImod ->
      fmap (PdtaBlockMod PdtaCatInst) (getPdtaElems label chunkLen sizeMod getMod)
    | label == labelIgen ->
      fmap (PdtaBlockGen PdtaCatInst) (getPdtaElems label chunkLen sizeGen getGen)
    | label == labelShdr ->
      fmap PdtaBlockShdr (getPdtaElems label chunkLen sizeShdr getShdr)
    | otherwise ->
      fail ("unrecognized pdta elem: " ++ show label)

decodePdtaBlocks :: Monad m => DecodeT m (Seq PdtaBlock)
decodePdtaBlocks = do
  remainingBytes <- decodeGet getPdtaHeader
  decodeRepeated remainingBytes (decodeGet getPdtaBlock)

encodeSfont :: Sfont -> BSL.ByteString
encodeSfont = runPut . putSfont

putSfont :: Sfont -> Put
putSfont sfont@(Sfont infos sdta pdtaBlocks) = result where
  result = do
    putByteString labelRiff
    putWord32le (fromIntegral (sizeSfont sfont))
    putByteString labelSfbk
    putInfos infos
    putSdta sdta
    putPdtaBlocks pdtaBlocks

sizeSfont :: Sfont -> ByteLength
sizeSfont (Sfont infos sdta pdtaBlocks) = 28 + sizeInfos infos + sizeSdta sdta + sizePdtaBlocks pdtaBlocks

sizeInfos :: Seq Info -> ByteLength
sizeInfos infos = 4 + getSum (foldMap' (\info -> Sum (sizeInfo info) + 8) infos)

sizePdtaBlocks :: Seq PdtaBlock -> ByteLength
sizePdtaBlocks pdtaBlocks = 4 + getSum (foldMap' (\block -> Sum (sizePdtaBlock block) + 8) pdtaBlocks)

sizeInfo :: Info -> ByteLength
sizeInfo = \case
  InfoVersion _ _ -> 4
  InfoTargetSoundEngine z -> sizeZstr z
  InfoBankName z -> sizeZstr z
  InfoRomName z -> sizeZstr z
  InfoRomVersion _ _ -> 4
  InfoCreationDate z -> sizeZstr z
  InfoAuthors z -> sizeZstr z
  InfoIntendedProduct z -> sizeZstr z
  InfoCopyrightMessage z -> sizeZstr z
  InfoComments z -> sizeZstr z
  InfoUsedTools z -> sizeZstr z
  InfoReserved _ bs -> fromIntegral (BS.length bs)

sizeSdta :: Sdta -> ByteLength
sizeSdta (Sdta high mlow) = sizeHigh + sizeLow where
  sizeHigh = 12 + 2 * fromIntegral (wavDataSamples high)
  sizeLow = maybe 0 (\low -> 8 + fromIntegral (wavDataSamples low)) mlow

sizePdtaBlock :: PdtaBlock -> ByteLength
sizePdtaBlock = \case
  PdtaBlockPhdr phdrs -> sizePhdr * fromIntegral (Seq.length phdrs)
  PdtaBlockBag _ bags -> sizeBag * fromIntegral (Seq.length bags)
  PdtaBlockMod _ mods -> sizeMod * fromIntegral (Seq.length mods)
  PdtaBlockGen _ gens -> sizeGen * fromIntegral (Seq.length gens)
  PdtaBlockInst insts -> sizeInst * fromIntegral (Seq.length insts)
  PdtaBlockShdr shdrs -> sizeShdr * fromIntegral (Seq.length shdrs)

putInfos :: Seq Info -> Put
putInfos infos = do
  putByteString labelList
  putWord32le (fromIntegral (sizeInfos infos))
  putByteString labelInfo
  putSeq putInfo infos

whichLabelInfo :: Info -> ByteString
whichLabelInfo = \case
  InfoVersion _ _ -> labelIfil
  InfoRomVersion _ _ -> labelIver
  InfoTargetSoundEngine _ -> labelIsng
  InfoBankName _ -> labelInam
  InfoRomName _ -> labelIrom
  InfoCreationDate _ -> labelIcrd
  InfoAuthors _ -> labelIeng
  InfoIntendedProduct _ -> labelIprd
  InfoCopyrightMessage _ -> labelIcop
  InfoComments _ -> labelIcmt
  InfoUsedTools _ -> labelIsft
  InfoReserved l _ -> l

putInfo :: Info -> Put
putInfo info = do
  putByteString (whichLabelInfo info)
  putWord32le (fromIntegral (sizeInfo info))
  case info of
    InfoVersion w1 w2 -> putWord16le w1 *> putWord16le w2
    InfoTargetSoundEngine z -> putZstr z
    InfoBankName z -> putZstr z
    InfoRomName z -> putZstr z
    InfoRomVersion w1 w2 -> putWord16le w1 *> putWord16le w2
    InfoCreationDate z -> putZstr z
    InfoAuthors z -> putZstr z
    InfoIntendedProduct z -> putZstr z
    InfoCopyrightMessage z -> putZstr z
    InfoComments z -> putZstr z
    InfoUsedTools z -> putZstr z
    InfoReserved _ bs -> putByteString bs

putSdta :: Sdta -> Put
putSdta sdta@(Sdta highBits mayLowBits) = do
  putByteString labelList
  putWord32le (fromIntegral (sizeSdta sdta))
  putByteString labelSdta
  putByteString labelSmpl
  putWord32le (fromIntegral (wavDataSamples highBits * 2))
  putVec putInt16le (unWavData highBits)
  case mayLowBits of
    Nothing -> pure ()
    Just lowBits -> do
      putByteString labelSm24
      putWord32le (fromIntegral (wavDataSamples lowBits))
      putVec putWord8 (unWavData lowBits)

putPdtaBlocks :: Seq PdtaBlock -> Put
putPdtaBlocks pdtaBlocks = do
  putByteString labelList
  putWord32le (fromIntegral (sizePdtaBlocks pdtaBlocks))
  putByteString labelPdta
  putSeq putPdtaBlock pdtaBlocks

whichLabelPdtaBlock :: PdtaBlock -> ByteString
whichLabelPdtaBlock = \case
  PdtaBlockPhdr _ -> labelPhdr
  PdtaBlockBag pc _ -> case pc of
    PdtaCatPreset -> labelPbag
    PdtaCatInst -> labelIbag
  PdtaBlockMod pc _ -> case pc of
    PdtaCatPreset -> labelPmod
    PdtaCatInst -> labelImod
  PdtaBlockGen pc _ -> case pc of
    PdtaCatPreset -> labelPgen
    PdtaCatInst -> labelIgen
  PdtaBlockInst _ -> labelInst
  PdtaBlockShdr _ -> labelShdr

putPdtaBlock :: PdtaBlock -> Put
putPdtaBlock block = do
  putByteString (whichLabelPdtaBlock block)
  putWord32le (fromIntegral (sizePdtaBlock block))
  case block of
    PdtaBlockPhdr phdrs -> putSeq putPhdr phdrs
    PdtaBlockBag _ bags -> putSeq putBag bags
    PdtaBlockMod _ mods -> putSeq putMod mods
    PdtaBlockGen _ gens -> putSeq putGen gens
    PdtaBlockInst insts -> putSeq putInst insts
    PdtaBlockShdr shdrs -> putSeq putShdr shdrs
