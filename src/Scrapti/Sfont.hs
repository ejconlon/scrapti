{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Sfont
  ( ListChunk (..)
  , OptChunk (..)
  , Sfont (..)
  , InfoChunk (..)
  , PdtaChunk (..)
  , SdtaChunk (..)
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
  , labelSfbk
  ) where

import Control.Monad (unless)
import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, Int16LE, PrimArray, ShortByteString, StaticByteSized (..),
               StaticBytes, TermBytes, ViaStaticByteSized (..), ViaStaticGeneric (..), Word16LE, Word32LE,
               byteSizeFoldable, getExact, getRemainingSeq, getRemainingSize, getRemainingStaticSeq, getRemainingString,
               getSkip, getStaticArray, putByteString, putSeq, putStaticArray)
import Data.Foldable (foldl')
import Data.Int (Int8)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import GHC.Generics (Generic)
import Scrapti.Riff (Label, StaticLabel (..), chunkHeaderSize, getChunkSize, getExpectLabel, labelRiff, labelSize,
                     putChunkSize)

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

newtype SampleCount = SampleCount { unSampleCount :: Word32LE }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, ByteSized, StaticByteSized, Binary)

type ShortText = StaticBytes 20

listChunkHeaderSize :: ByteCount
listChunkHeaderSize = chunkHeaderSize + labelSize

newtype ListChunk a = ListChunk { listChunkElems :: Seq a }
  deriving stock (Show)
  deriving newtype (Eq)

instance ByteSized a => ByteSized (ListChunk a) where
  byteSize (ListChunk elems) = listChunkHeaderSize + byteSizeFoldable elems

instance (StaticLabel a, Binary a) => Binary (ListChunk a) where
  get = do
    getExpectLabel labelList
    chunkSize <- getChunkSize
    getExact chunkSize $ do
      let !label = staticLabel (Proxy :: Proxy a)
      getExpectLabel label
      elems <- getRemainingSeq get
      pure $! ListChunk elems
  put (ListChunk elems) = do
    put labelList
    let !chunkSize = byteSizeFoldable elems + labelSize
    putChunkSize chunkSize
    let !label = staticLabel (Proxy :: Proxy a)
    put label
    putSeq put elems

newtype OptChunk a = OptChunk { optChunkElem :: Maybe a }
  deriving stock (Show)
  deriving newtype (Eq)

instance ByteSized a => ByteSized (OptChunk a) where
  byteSize (OptChunk mayElem) = listChunkHeaderSize + byteSizeFoldable mayElem

instance (StaticLabel a, Binary a) => Binary (OptChunk a) where
  get = do
    getExpectLabel labelList
    chunkSize <- getChunkSize
    getExact chunkSize $ do
      let !label = staticLabel (Proxy :: Proxy a)
      getExpectLabel label
      mayElem <-
        if chunkSize == labelSize
          then pure Nothing
          else fmap Just get
      pure $! OptChunk mayElem
  put (OptChunk mayElem) = do
    put labelList
    let !chunkSize = byteSizeFoldable mayElem + labelSize
    putChunkSize chunkSize
    let !label = staticLabel (Proxy :: Proxy a)
    put label
    maybe (pure ()) put mayElem

data Sfont = Sfont
  { sfontInfo :: !InfoChunk
  , sfontSdta :: !SdtaChunk
  , sfontPdta :: !PdtaChunk
  } deriving stock (Eq, Show)

instance ByteSized Sfont where
  byteSize (Sfont info sdta pdta) = chunkHeaderSize + labelSize + byteSize info + byteSize sdta + byteSize pdta

instance Binary Sfont where
  get = do
    getExpectLabel labelRiff
    chunkSize <- getChunkSize
    getExact chunkSize $ do
      getExpectLabel labelSfbk
      info <- get
      sdta <- get
      pdta <- get
      pure $! Sfont info sdta pdta
  put sfont@(Sfont info sdta pdta) = do
    put labelRiff
    let !chunkSize = byteSize sfont - chunkHeaderSize
    putChunkSize chunkSize
    put labelSfbk
    put info
    put sdta
    put pdta

newtype InfoChunk = InfoChunk { unInfoChunk :: ListChunk Info }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, Binary)

newtype PdtaChunk = PdtaChunk { unPdtaChunk :: ListChunk PdtaBlock }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, Binary)

data Info =
    InfoVersion !Word16LE !Word16LE
  | InfoTargetSoundEngine !TermBytes
  | InfoBankName !TermBytes
  | InfoRomName !TermBytes
  | InfoRomVersion !Word16LE !Word16LE
  | InfoCreationDate !TermBytes
  | InfoAuthors !TermBytes
  | InfoIntendedProduct !TermBytes
  | InfoCopyrightMessage !TermBytes
  | InfoComments !TermBytes
  | InfoUsedTools !TermBytes
  | InfoReserved !Label !ShortByteString
  deriving stock (Eq, Show)

instance StaticLabel Info where
  staticLabel _ = labelInfo

instance ByteSized Info where
  byteSize info = chunkHeaderSize + case info of
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
    InfoReserved _ bs -> byteSize bs

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

instance Binary Info where
  get = do
    label <- get
    chunkSize <- getChunkSize
    getExact chunkSize $
      if
        | label == labelIfil -> do
          unless (chunkSize == 4) (fail "Bad ifil chunk size")
          w1 <- get
          w2 <- get
          pure $! InfoVersion w1 w2
        | label == labelIver -> do
          unless (chunkSize == 4) (fail "Bad iver chunk size")
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
          bs <- getRemainingString
          pure $! InfoReserved label bs
  put info = do
    let !label = whichLabelInfo info
    put label
    let !chunkSize = byteSize info - chunkHeaderSize
    putChunkSize chunkSize
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
      InfoReserved _ bs -> putByteString bs

data Sdta = Sdta
  { sdtaHighBits :: !(PrimArray Int16LE)
  , sdtaLowBits :: !(Maybe (PrimArray Word8))
  } deriving stock (Eq, Show)

instance ByteSized Sdta where
  byteSize (Sdta high mlow) = sizeHigh + sizeLow where
    sizeHigh = chunkHeaderSize + byteSize high
    sizeLow = maybe 0 (\low -> chunkHeaderSize + byteSize low) mlow

instance StaticLabel Sdta where
  staticLabel _ = labelSdta

getHighBits :: SampleCount -> Get (PrimArray Int16LE)
getHighBits numSamples = getStaticArray (fromIntegral numSamples)

getLowBits :: SampleCount -> Get (PrimArray Word8)
getLowBits numSamples = getStaticArray (fromIntegral numSamples)

instance Binary Sdta where
  get = do
    chunkSize <- getRemainingSize
    getExpectLabel labelSmpl
    highSize <- getChunkSize
    let !numSamples = div (fromIntegral highSize) 2
    highBits <- getHighBits numSamples
    let !numExtra = chunkSize - highSize - chunkHeaderSize
    if
      | numExtra > 0 -> do
        getExpectLabel labelSm24
        lowSize <- getChunkSize
        let !expectedSize = if even numSamples then numSamples else numSamples + 1
        unless (fromIntegral lowSize == expectedSize) (fail "invalid low sample size")
        lowBits <- getLowBits numSamples
        unless (even numSamples) (getSkip 1)
        pure $! Sdta highBits (Just lowBits)
      | numExtra == 0 -> pure $! Sdta highBits Nothing
      | otherwise -> fail "invalid sdata chunk/sample sizes"
  put (Sdta highBits mayLowBits) = do
    put labelSmpl
    putChunkSize (byteSize highBits)
    putStaticArray highBits
    case mayLowBits of
      Nothing -> pure ()
      Just lowBits -> do
        put labelSm24
        putChunkSize (byteSize lowBits)
        putStaticArray lowBits

newtype SdtaChunk = SdtaChunk { unSdtaChunk :: OptChunk Sdta }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, Binary)

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

instance StaticLabel PdtaBlock where
  staticLabel _ = labelPdta

instance ByteSized PdtaBlock where
  byteSize block = res where
    res = chunkHeaderSize + case block of
      PdtaBlockPhdr phdrs -> fromIntegral (Seq.length phdrs) * sizePhdr
      PdtaBlockBag _ bags -> fromIntegral (Seq.length bags) * sizeBag
      PdtaBlockMod _ mods -> fromIntegral (Seq.length mods) * sizeMod
      PdtaBlockGen _ gens -> fromIntegral (Seq.length gens) * sizeGen
      PdtaBlockInst insts -> fromIntegral (Seq.length insts) * sizeInst
      PdtaBlockShdr shdrs -> fromIntegral (Seq.length shdrs) * sizeShdr
    sizePhdr = staticByteSize (Proxy :: Proxy Phdr)
    sizeBag = staticByteSize (Proxy :: Proxy Bag)
    sizeMod = staticByteSize (Proxy :: Proxy Mod)
    sizeGen = staticByteSize (Proxy :: Proxy Gen)
    sizeInst = staticByteSize (Proxy :: Proxy Inst)
    sizeShdr = staticByteSize (Proxy :: Proxy Shdr)

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

instance Binary PdtaBlock where
  get = do
    label <- get
    chunkSize <- getChunkSize
    getExact chunkSize $
      if
        | label == labelPhdr ->
          fmap PdtaBlockPhdr (getRemainingStaticSeq get)
        | label == labelPbag ->
          fmap (PdtaBlockBag PdtaCatPreset) (getRemainingStaticSeq get)
        | label == labelPmod ->
          fmap (PdtaBlockMod PdtaCatPreset) (getRemainingStaticSeq get)
        | label == labelPgen ->
          fmap (PdtaBlockGen PdtaCatPreset) (getRemainingStaticSeq get)
        | label == labelInst ->
          fmap PdtaBlockInst (getRemainingSeq get)
        | label == labelIbag ->
          fmap (PdtaBlockBag PdtaCatInst) (getRemainingStaticSeq get)
        | label == labelImod ->
          fmap (PdtaBlockMod PdtaCatInst) (getRemainingStaticSeq get)
        | label == labelIgen ->
          fmap (PdtaBlockGen PdtaCatInst) (getRemainingStaticSeq get)
        | label == labelShdr ->
          fmap PdtaBlockShdr (getRemainingStaticSeq get)
        | otherwise ->
          fail ("unrecognized pdta elem: " ++ show label)
  put block = do
    let !label = whichLabelPdtaBlock block
    put label
    let !chunkSize = byteSize block - chunkHeaderSize
    putChunkSize chunkSize
    case block of
      PdtaBlockPhdr phdrs -> putSeq put phdrs
      PdtaBlockBag _ bags -> putSeq put bags
      PdtaBlockMod _ mods -> putSeq put mods
      PdtaBlockGen _ gens -> putSeq put gens
      PdtaBlockInst insts -> putSeq put insts
      PdtaBlockShdr shdrs -> putSeq put shdrs

-- | Preset header
data Phdr = Phdr
  { phdrPresetName :: !ShortText
  , phdrPreset :: !Word16LE
  , phdrBank :: !Word16LE
  , phdrPresetBagIndex :: !Word16LE
  , phdrLibrary :: !Word32LE
  , phdrGenre :: !Word32LE
  , phdrMorphology :: !Word32LE
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Phdr)

data Bag = Bag
  { bagGenIndex :: !Word16LE
  , bagModIndex :: !Word16LE
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Bag)

-- | Modulator
data Mod = Mod
  { modSrcOper :: !Word16LE
  , modDestOper :: !Word16LE
  , modAmount :: !Int16LE
  , modAmtSrcOper :: !Word16LE
  , modTransOper :: !Word16LE
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Mod)

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
  deriving (ByteSized) via (ViaStaticByteSized Gen)

instance StaticByteSized Gen where
  staticByteSize _ = 4

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

instance Binary Gen where
  get = do
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
  put gen = do
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

-- | Instrument
data Inst = Inst
  { instName :: !ShortText
  , instBagIndex :: !Word16LE
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Inst)

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
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Shdr)

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
