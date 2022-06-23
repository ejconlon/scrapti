{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (foldMap', foldl')
import Data.Int (Int8)
import Data.Semigroup (Sum (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import Scrapti.Binary (Binary (..), ByteLength, DecodeT, FixedText, Get, Int16LE, Put, SizedBinary (..), TermText,
                       Word16LE, Word32LE, decodeBounded, decodeGet, decodeRepeated, getByteString, getSeq, getVec,
                       guardEnd, putSeq, putVec, runPut, skip)
import Scrapti.Riff (Label, expectLabel, getChunkSize, labelRiff, putChunkSize)
import Scrapti.Wav (WavData (..), wavDataSamples)

newtype SampleCount = Samplecount { unSampleCount :: Word32LE }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Binary)

type ShortText = FixedText 20

data Sfont = Sfont
  { sfontInfos :: !(Seq Info)
  , sfontSdta :: !Sdta
  , sfontPdta :: !(Seq PdtaBlock)
  } deriving stock (Eq, Show)

data Info =
    InfoVersion !Word16LE !Word16LE
  | InfoTargetSoundEngine !TermText
  | InfoBankName !TermText
  | InfoRomName !TermText
  | InfoRomVersion !Word16LE !Word16LE
  | InfoCreationDate !TermText
  | InfoAuthors !TermText
  | InfoIntendedProduct !TermText
  | InfoCopyrightMessage !TermText
  | InfoComments !TermText
  | InfoUsedTools !TermText
  | InfoReserved !Label !ByteString
  deriving stock (Eq, Show)

data Sdta = Sdta
  { sdtaHighBits :: !(WavData Int16LE)
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
  { phdrPresetName :: !ShortText
  , phdrPreset :: !Word16LE
  , phdrBank :: !Word16LE
  , phdrPresetBagIndex :: !Word16LE
  , phdrLibrary :: !Word32LE
  , phdrGenre :: !Word32LE
  , phdrMorphology :: !Word32LE
  } deriving stock (Eq, Show)

data Bag = Bag
  { bagGenIndex :: !Word16LE
  , bagModIndex :: !Word16LE
  } deriving stock (Eq, Show)

-- | Modulator
data Mod = Mod
  { modSrcOper :: !Word16LE
  , modDestOper :: !Word16LE
  , modAmount :: !Int16LE
  , modAmtSrcOper :: !Word16LE
  , modTransOper :: !Word16LE
  } deriving stock (Eq, Show)

data SampleMode =
    SampleModeNoLoop !Int16LE
  | SampleModeContLoop
  | SampleModePressLoop
  deriving stock (Eq, Show)

-- | Generator
data Gen =
    GenStartAddressOffset !Int16LE
  | GenEndAddressOffset !Int16LE
  | GenLoopStartAddressOffset !Int16LE
  | GenLoopEndAddressOffset !Int16LE
  | GenStartAddressCoarseOffset !Int16LE
  | GenModLfoToPitch !Int16LE
  | GenVibLfoToPitch !Int16LE
  | GenModEnvToPitch !Int16LE
  | GenInitFc !Int16LE
  | GenInitQ !Int16LE
  | GenModLfoToFc !Int16LE
  | GenModEnvToFc !Int16LE
  | GenEndAddressCoarseOffset !Int16LE
  | GenModLfoToVol !Int16LE
  | GenChorus !Int16LE
  | GenReverb !Int16LE
  | GenPan !Int16LE
  | GenDelayModLfo !Int16LE
  | GenFreqModLfo !Int16LE
  | GenDelayVibLfo !Int16LE
  | GenFreqVibLfo !Int16LE
  | GenDelayModEnv !Int16LE
  | GenAttackModEnv !Int16LE
  | GenHoldModEnv !Int16LE
  | GenDecayModEnv !Int16LE
  | GenSustainModEnv !Int16LE
  | GenReleaseModEnv !Int16LE
  | GenKeyToModEnvHold !Int16LE
  | GenKeyToModEnvDecay !Int16LE
  | GenDelayVolEnv !Int16LE
  | GenAttackVolEnv !Int16LE
  | GenHoldVolEnv !Int16LE
  | GenDecayVolEnv !Int16LE
  | GenSustainVolEnv !Int16LE
  | GenReleaseVolEnv !Int16LE
  | GenKeyToVolEnvHold !Int16LE
  | GenKeyToVolEnvDecay !Int16LE
  | GenInstIndex !Word16LE
  | GenKeyRange !Word8 !Word8
  | GenVelRange !Word8 !Word8
  | GenLoopStartAddressCoarseOffset !Int16LE
  | GenKey !Word16LE
  | GenVel !Word16LE
  | GenInitAtten !Int16LE
  | GenLoopEndAddressCoarseOffset !Int16LE
  | GenCoarseTune !Int16LE
  | GenFineTune !Int16LE
  | GenSampleIndex !Word16LE
  | GenSampleMode !SampleMode
  | GenScaleTuning !Int16LE
  | GenExclusiveClass !Int16LE
  | GenRootKey !Word16LE
  | GenReserved !Int16LE !Int16LE
  deriving stock (Eq, Show)

-- | Instrument
data Inst = Inst
  { instName :: !ShortText
  , instBagIndex :: !Word16LE
  } deriving stock (Eq, Show)

-- | Sample header
data Shdr = Shdr
  { shdrSampleName :: !ShortText
  , shdrStart :: !Word32LE
  , shdrEnd :: !Word32LE
  , shdrStartLoop :: !Word32LE
  , shdrEndLoop :: !Word32LE
  , shdrSampleRate :: !Word32LE
  , shdrOriginalPitch :: !Word8
  , shdrPitchCorrection :: !Int8
  , shdrSampleLink :: !Word16LE
  , shdrSampleType :: !Word16LE
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
  labelPdta, labelPhdr, labelPbag, labelPmod, labelPgen, labelInst, labelIbag, labelImod, labelIgen, labelShdr :: Label
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
  chunkSize <- getChunkSize
  expectLabel labelSfbk
  pure $! chunkSize - 4

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
  chunkSize <- getChunkSize
  expectLabel labelInfo
  pure $! chunkSize - 4

getInfo :: Get Info
getInfo = do
  label <- get
  chunkSize <- getChunkSize
  if
    | label == labelIfil -> do
      unless (chunkSize == 4) (fail "bad ifil chunk size")
      w1 <- get
      w2 <- get
      pure $! InfoVersion w1 w2
    | label == labelIver -> do
      unless (chunkSize == 4) (fail "bad iver chunk size")
      w1 <- get
      w2 <- get
      pure $! InfoRomVersion w1 w2
    | label == labelIsng -> fmap InfoTargetSoundEngine get
    | label == labelInam -> fmap InfoBankName get
    | label == labelIrom -> fmap InfoRomName get
    | label == labelIcrd -> fmap InfoCreationDate get
    | label == labelIeng -> fmap InfoAuthors get
    | label == labelIprd -> fmap InfoIntendedProduct get
    | label == labelIcop -> fmap InfoCopyrightMessage get
    | label == labelIcmt -> fmap InfoComments get
    | label == labelIsft -> fmap InfoUsedTools get
    | otherwise -> do
      bs <- getByteString (fromIntegral chunkSize)
      pure $! InfoReserved label bs

decodeInfos :: Monad m => DecodeT m (Seq Info)
decodeInfos = do
  remainingSize <- decodeGet getInfosHeader
  decodeRepeated remainingSize (decodeGet getInfo)

getHighBits :: SampleCount -> Get (WavData Int16LE)
getHighBits numSamples = fmap WavData (getVec (fromIntegral numSamples) get)

getLowBits :: SampleCount -> Get (WavData Word8)
getLowBits numSamples = fmap WavData (getVec (fromIntegral numSamples) get)

getSdta :: Get Sdta
getSdta = do
  expectLabel labelList
  chunkSize <- getChunkSize
  expectLabel labelSdta
  expectLabel labelSmpl
  highSize <- getChunkSize
  let !numSamples = div (fromIntegral highSize) 2
  highBits <- getHighBits numSamples
  let !numExtra = chunkSize - highSize - 12
  if
    | numExtra > 0 -> do
      expectLabel labelSm24
      lowSize <- getChunkSize
      let !expectedSize = if even numSamples then numSamples else numSamples + 1
      unless (fromIntegral lowSize == expectedSize) (fail "invalid low sample size")
      lowBits <- getLowBits numSamples
      unless (even numSamples) (skip 1)
      pure $! Sdta highBits (Just lowBits)
    | numExtra == 0 -> pure $! Sdta highBits Nothing
    | otherwise -> fail "invalid sdata chunk/sample sizes"

getPdtaHeader :: Get ByteLength
getPdtaHeader = do
  expectLabel labelList
  chunkSize <- getChunkSize
  expectLabel labelPdta
  pure $! chunkSize - 4

sizePhdr, sizeBag, sizeMod, sizeGen, sizeInst, sizeShdr :: ByteLength
sizePhdr = 38
sizeBag = 4
sizeMod = 10
sizeGen = 4
sizeInst = 22
sizeShdr = 46

getPdtaElems :: Label -> ByteLength -> ByteLength -> Get a -> Get (Seq a)
getPdtaElems label chunkLen size getter = do
  unless (mod chunkLen size == 0) (fail ("invalid size for pdta elem: " ++ show label))
  let !numElems = div chunkLen size
  getSeq (fromIntegral numElems) getter

getPhdr :: Get Phdr
getPhdr = do
  phdrPresetName <- get
  phdrPreset <- get
  phdrBank <- get
  phdrPresetBagIndex <- get
  phdrLibrary <- get
  phdrGenre <- get
  phdrMorphology <- get
  pure $! Phdr {..}

putPhdr :: Phdr -> Put
putPhdr (Phdr {..}) = do
  put phdrPresetName
  put phdrPreset
  put phdrBank
  put phdrPresetBagIndex
  put phdrLibrary
  put phdrGenre
  put phdrMorphology

getBag :: Get Bag
getBag = do
  bagGenIndex <- get
  bagModIndex <- get
  pure $! Bag {..}

putBag :: Bag -> Put
putBag (Bag {..})= do
  put bagGenIndex
  put bagModIndex

getMod :: Get Mod
getMod = do
  modSrcOper <- get
  modDestOper <- get
  modAmount <- get
  modAmtSrcOper <- get
  modTransOper <- get
  pure $! Mod {..}

putMod :: Mod -> Put
putMod (Mod {..}) = do
  put modSrcOper
  put modDestOper
  put modAmount
  put modAmtSrcOper
  put modTransOper

getGen :: Get Gen
getGen = do
  tag <- get
  if
    | tag == 0 -> fmap GenStartAddressOffset get
    | tag == 1 -> fmap GenEndAddressOffset get
    | tag == 2 -> fmap GenLoopStartAddressOffset get
    | tag == 3 -> fmap GenLoopEndAddressOffset get
    | tag == 4 -> fmap GenStartAddressCoarseOffset get
    | tag == 5 -> fmap GenModLfoToPitch get
    | tag == 6 -> fmap GenVibLfoToPitch get
    | tag == 7 -> fmap GenModEnvToPitch get
    | tag == 8 -> fmap GenInitFc get
    | tag == 9 -> fmap GenInitQ get
    | tag == 10 -> fmap GenModLfoToFc get
    | tag == 11 -> fmap GenModEnvToFc get
    | tag == 12 -> fmap GenEndAddressCoarseOffset get
    | tag == 13 -> fmap GenModLfoToVol get
    | tag == 15 -> fmap GenChorus get
    | tag == 16 -> fmap GenReverb get
    | tag == 17 -> fmap GenPan get
    | tag == 21 -> fmap GenDelayModLfo get
    | tag == 22 -> fmap GenFreqModLfo get
    | tag == 23 -> fmap GenDelayVibLfo get
    | tag == 24 -> fmap GenFreqVibLfo get
    | tag == 25 -> fmap GenDelayModEnv get
    | tag == 26 -> fmap GenAttackModEnv get
    | tag == 27 -> fmap GenHoldModEnv get
    | tag == 28 -> fmap GenDecayModEnv get
    | tag == 29 -> fmap GenSustainModEnv get
    | tag == 30 -> fmap GenReleaseModEnv get
    | tag == 31 -> fmap GenKeyToModEnvHold get
    | tag == 32 -> fmap GenKeyToModEnvDecay get
    | tag == 33 -> fmap GenDelayVolEnv get
    | tag == 34 -> fmap GenAttackVolEnv get
    | tag == 35 -> fmap GenHoldVolEnv get
    | tag == 36 -> fmap GenDecayVolEnv get
    | tag == 37 -> fmap GenSustainVolEnv get
    | tag == 38 -> fmap GenReleaseVolEnv get
    | tag == 39 -> fmap GenKeyToVolEnvHold get
    | tag == 40 -> fmap GenKeyToVolEnvDecay get
    | tag == 41 -> fmap GenInstIndex get
    | tag == 43 -> do
      a <- get
      b <- get
      pure $! GenKeyRange a b
    | tag == 44 -> do
      a <- get
      b <- get
      pure $! GenVelRange a b
    | tag == 45 -> fmap GenLoopStartAddressCoarseOffset get
    | tag == 46 -> fmap GenKey get
    | tag == 47 -> fmap GenVel get
    | tag == 48 -> fmap GenInitAtten get
    | tag == 50 -> fmap GenLoopEndAddressCoarseOffset get
    | tag == 51 -> fmap GenCoarseTune get
    | tag == 52 -> fmap GenFineTune get
    | tag == 53 -> fmap GenSampleIndex get
    | tag == 54 -> do
      a <- get
      let !sm = case a of
            1 -> SampleModeContLoop
            3 -> SampleModePressLoop
            _ -> SampleModeNoLoop a
      pure $! GenSampleMode sm
    | tag == 56 -> fmap GenScaleTuning get
    | tag == 57 -> fmap GenExclusiveClass get
    | tag == 58 -> fmap GenRootKey get
    | otherwise -> do
      a <- get
      pure $! GenReserved tag a

whichTagGen :: Gen -> Int16LE
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
  put (whichTagGen gen)
  case gen of
    GenStartAddressOffset x -> put x
    GenEndAddressOffset x -> put x
    GenLoopStartAddressOffset x -> put x
    GenLoopEndAddressOffset x -> put x
    GenStartAddressCoarseOffset x -> put x
    GenModLfoToPitch x -> put x
    GenVibLfoToPitch x -> put x
    GenModEnvToPitch x -> put x
    GenInitFc x -> put x
    GenInitQ x -> put x
    GenModLfoToFc x -> put x
    GenModEnvToFc x -> put x
    GenEndAddressCoarseOffset x -> put x
    GenModLfoToVol x -> put x
    GenChorus x -> put x
    GenReverb x -> put x
    GenPan x -> put x
    GenDelayModLfo x -> put x
    GenFreqModLfo x -> put x
    GenDelayVibLfo x -> put x
    GenFreqVibLfo x -> put x
    GenDelayModEnv x -> put x
    GenAttackModEnv x -> put x
    GenHoldModEnv x -> put x
    GenDecayModEnv x -> put x
    GenSustainModEnv x -> put x
    GenReleaseModEnv x -> put x
    GenKeyToModEnvHold x -> put x
    GenKeyToModEnvDecay x -> put x
    GenDelayVolEnv x -> put x
    GenAttackVolEnv x -> put x
    GenHoldVolEnv x -> put x
    GenDecayVolEnv x -> put x
    GenSustainVolEnv x -> put x
    GenReleaseVolEnv x -> put x
    GenKeyToVolEnvHold x -> put x
    GenKeyToVolEnvDecay x -> put x
    GenInstIndex x -> put x
    GenKeyRange x y -> put x *> put y
    GenVelRange x y -> put x *> put y
    GenLoopStartAddressCoarseOffset x -> put x
    GenKey x -> put x
    GenVel x -> put x
    GenInitAtten x -> put x
    GenLoopEndAddressCoarseOffset x -> put x
    GenCoarseTune x -> put x
    GenFineTune x -> put x
    GenSampleIndex x -> put x
    GenSampleMode sm ->
      let !x = case sm of
            SampleModeContLoop -> 1
            SampleModePressLoop -> 3
            SampleModeNoLoop c -> c
      in put x
    GenScaleTuning x -> put x
    GenExclusiveClass x -> put x
    GenRootKey x -> put x
    GenReserved _ x -> put x

getInst :: Get Inst
getInst = do
  instName <- get
  instBagIndex <- get
  pure $! Inst {..}

putInst :: Inst -> Put
putInst (Inst {..}) = do
  put instName
  put instBagIndex

getShdr :: Get Shdr
getShdr = do
  shdrSampleName <- get
  shdrStart <- get
  shdrEnd <- get
  shdrStartLoop <- get
  shdrEndLoop <- get
  shdrSampleRate <- get
  shdrOriginalPitch <- get
  shdrPitchCorrection <- get
  shdrSampleLink <- get
  shdrSampleType <- get
  pure $! Shdr {..}

putShdr :: Shdr -> Put
putShdr (Shdr {..}) = do
  put shdrSampleName
  put shdrStart
  put shdrEnd
  put shdrStartLoop
  put shdrEndLoop
  put shdrSampleRate
  put shdrOriginalPitch
  put shdrPitchCorrection
  put shdrSampleLink
  put shdrSampleType

getPdtaBlock :: Get PdtaBlock
getPdtaBlock = do
  label <- get
  chunkSize <- getChunkSize
  if
    | label == labelPhdr ->
      fmap PdtaBlockPhdr (getPdtaElems label chunkSize sizePhdr getPhdr)
    | label == labelPbag ->
      fmap (PdtaBlockBag PdtaCatPreset) (getPdtaElems label chunkSize sizeBag getBag)
    | label == labelPmod ->
      fmap (PdtaBlockMod PdtaCatPreset) (getPdtaElems label chunkSize sizeMod getMod)
    | label == labelPgen ->
      fmap (PdtaBlockGen PdtaCatPreset) (getPdtaElems label chunkSize sizeGen getGen)
    | label == labelInst ->
      fmap PdtaBlockInst (getPdtaElems label chunkSize sizeInst getInst)
    | label == labelIbag ->
      fmap (PdtaBlockBag PdtaCatInst) (getPdtaElems label chunkSize sizeBag getBag)
    | label == labelImod ->
      fmap (PdtaBlockMod PdtaCatInst) (getPdtaElems label chunkSize sizeMod getMod)
    | label == labelIgen ->
      fmap (PdtaBlockGen PdtaCatInst) (getPdtaElems label chunkSize sizeGen getGen)
    | label == labelShdr ->
      fmap PdtaBlockShdr (getPdtaElems label chunkSize sizeShdr getShdr)
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
    put labelRiff
    putChunkSize (sizeSfont sfont)
    put labelSfbk
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
  InfoTargetSoundEngine z -> byteSize z
  InfoBankName z -> byteSize z
  InfoRomName z -> byteSize z
  InfoRomVersion _ _ -> 4
  InfoCreationDate z -> byteSize z
  InfoAuthors z -> byteSize z
  InfoIntendedProduct z -> byteSize z
  InfoCopyrightMessage z -> byteSize z
  InfoComments z -> byteSize z
  InfoUsedTools z -> byteSize z
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
  put labelList
  putChunkSize (sizeInfos infos)
  put labelInfo
  putSeq putInfo infos

whichLabelInfo :: Info -> Label
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
  put (whichLabelInfo info)
  putChunkSize (sizeInfo info)
  case info of
    InfoVersion w1 w2 -> put w1 *> put w2
    InfoTargetSoundEngine z -> put z
    InfoBankName z -> put z
    InfoRomName z -> put z
    InfoRomVersion w1 w2 -> put w1 *> put w2
    InfoCreationDate z -> put z
    InfoAuthors z -> put z
    InfoIntendedProduct z -> put z
    InfoCopyrightMessage z -> put z
    InfoComments z -> put z
    InfoUsedTools z -> put z
    InfoReserved _ bs -> put bs

putSdta :: Sdta -> Put
putSdta sdta@(Sdta highBits mayLowBits) = do
  put labelList
  putChunkSize (sizeSdta sdta)
  put labelSdta
  put labelSmpl
  putChunkSize (fromIntegral (wavDataSamples highBits * 2))
  putVec put (unWavData highBits)
  case mayLowBits of
    Nothing -> pure ()
    Just lowBits -> do
      put labelSm24
      putChunkSize (fromIntegral (wavDataSamples lowBits))
      putVec put (unWavData lowBits)

putPdtaBlocks :: Seq PdtaBlock -> Put
putPdtaBlocks pdtaBlocks = do
  put labelList
  putChunkSize (sizePdtaBlocks pdtaBlocks)
  put labelPdta
  putSeq putPdtaBlock pdtaBlocks

whichLabelPdtaBlock :: PdtaBlock -> Label
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
  put (whichLabelPdtaBlock block)
  putChunkSize (sizePdtaBlock block)
  case block of
    PdtaBlockPhdr phdrs -> putSeq putPhdr phdrs
    PdtaBlockBag _ bags -> putSeq putBag bags
    PdtaBlockMod _ mods -> putSeq putMod mods
    PdtaBlockGen _ gens -> putSeq putGen gens
    PdtaBlockInst insts -> putSeq putInst insts
    PdtaBlockShdr shdrs -> putSeq putShdr shdrs
