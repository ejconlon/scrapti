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
  , GenPair (..)
  , GenTag (..)
  , defGenVal
  , lookupGenVal
  , Inst (..)
  , Shdr (..)
  , buildPdta
  , labelSfbk
  ) where

import Control.Monad (unless)
import Dahdit (Binary (..), ByteSized (..), Get, Int16LE (..), PrimArray, Put, ShortByteString, StaticByteSized (..),
               StaticBytes, TermBytes, ViaStaticByteSized (..), ViaStaticGeneric (..), Word16LE, Word32LE,
               byteSizeFoldable, getExact, getRemainingSeq, getRemainingSize, getRemainingStaticSeq, getRemainingString,
               getSkip, getStaticArray, putByteString, putSeq, putStaticArray)
import Data.Foldable (foldl')
import Data.Int (Int8)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Type.Equality (testEquality)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Scrapti.Riff (KnownLabel (..), Label, chunkHeaderSize, getChunkSize, getExpectLabel, labelList, labelRiff,
                     labelSize, listChunkHeaderSize, putChunkSize)
import Type.Reflection ((:~:) (..), TypeRep, Typeable, typeRep)

labelSfbk, labelInfo, labelIfil, labelIver, labelIsng, labelInam, labelIrom, labelIcrd,
  labelIeng, labelIprd, labelIcop, labelIcmt, labelIsft, labelSdta, labelSmpl, labelSm24,
  labelPdta, labelPhdr, labelPbag, labelPmod, labelPgen, labelInst, labelIbag, labelImod, labelIgen, labelShdr :: Label
labelSfbk = "sfbk"
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

newtype ListChunk a = ListChunk { listChunkElems :: Seq a }
  deriving stock (Show)
  deriving newtype (Eq)

instance ByteSized a => ByteSized (ListChunk a) where
  byteSize (ListChunk elems) = listChunkHeaderSize + byteSizeFoldable elems

instance (KnownLabel a, Binary a) => Binary (ListChunk a) where
  get = do
    getExpectLabel labelList
    chunkSize <- getChunkSize
    getExact chunkSize $ do
      let !label = knownLabel (Proxy :: Proxy a)
      getExpectLabel label
      elems <- getRemainingSeq get
      pure $! ListChunk elems
  put (ListChunk elems) = do
    put labelList
    let !chunkSize = byteSizeFoldable elems + labelSize
    putChunkSize chunkSize
    let !label = knownLabel (Proxy :: Proxy a)
    put label
    putSeq put elems

newtype OptChunk a = OptChunk { optChunkElem :: Maybe a }
  deriving stock (Show)
  deriving newtype (Eq)

instance ByteSized a => ByteSized (OptChunk a) where
  byteSize (OptChunk mayElem) = listChunkHeaderSize + byteSizeFoldable mayElem

instance (KnownLabel a, Binary a) => Binary (OptChunk a) where
  get = do
    getExpectLabel labelList
    chunkSize <- getChunkSize
    getExact chunkSize $ do
      let !label = knownLabel (Proxy :: Proxy a)
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
    let !label = knownLabel (Proxy :: Proxy a)
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

instance KnownLabel Info where
  knownLabel _ = labelInfo

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

instance KnownLabel Sdta where
  knownLabel _ = labelSdta

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

instance KnownLabel PdtaBlock where
  knownLabel _ = labelPdta

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
  deriving stock (Eq, Ord, Show)
  deriving (ByteSized) via (ViaStaticByteSized SampleMode)

instance StaticByteSized SampleMode where
  staticByteSize _ = 2

instance Binary SampleMode where
  get = do
    c <- get
    pure $! case c of
      1 -> SampleModeContLoop
      3 -> SampleModePressLoop
      _ -> SampleModeNoLoop c
  put sm =
    put $! case sm of
      SampleModeContLoop -> 1
      SampleModePressLoop -> 3
      SampleModeNoLoop c -> c

data Range = Range
  { rangeLo :: !Word8
  , rangeHi :: !Word8
  } deriving stock (Eq, Ord, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Range)

data ReservedGen = ReservedGen
  { reservedGenTag :: !Word16LE
  , reservedGetVal :: !Int16LE
  } deriving stock (Eq, Ord, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric ReservedGen)

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

-- | Tags for generators
data GenTag a where
  GenTagStartAddressOffset :: GenTag Int16LE
  GenTagEndAddressOffset :: GenTag Int16LE
  GenTagLoopStartAddressOffset :: GenTag Int16LE
  GenTagLoopEndAddressOffset :: GenTag Int16LE
  GenTagStartAddressCoarseOffset :: GenTag Int16LE
  GenTagModLfoToPitch :: GenTag Int16LE
  GenTagVibLfoToPitch :: GenTag Int16LE
  GenTagModEnvToPitch :: GenTag Int16LE
  GenTagInitFc :: GenTag Int16LE
  GenTagInitQ :: GenTag Int16LE
  GenTagModLfoToFc :: GenTag Int16LE
  GenTagModEnvToFc :: GenTag Int16LE
  GenTagEndAddressCoarseOffset :: GenTag Int16LE
  GenTagModLfoToVol :: GenTag Int16LE
  GenTagChorus :: GenTag Int16LE
  GenTagReverb :: GenTag Int16LE
  GenTagPan :: GenTag Int16LE
  GenTagDelayModLfo :: GenTag Int16LE
  GenTagFreqModLfo :: GenTag Int16LE
  GenTagDelayVibLfo :: GenTag Int16LE
  GenTagFreqVibLfo :: GenTag Int16LE
  GenTagDelayModEnv :: GenTag Int16LE
  GenTagAttackModEnv :: GenTag Int16LE
  GenTagHoldModEnv :: GenTag Int16LE
  GenTagDecayModEnv :: GenTag Int16LE
  GenTagSustainModEnv :: GenTag Int16LE
  GenTagReleaseModEnv :: GenTag Int16LE
  GenTagKeyToModEnvHold :: GenTag Int16LE
  GenTagKeyToModEnvDecay :: GenTag Int16LE
  GenTagDelayVolEnv :: GenTag Int16LE
  GenTagAttackVolEnv :: GenTag Int16LE
  GenTagHoldVolEnv :: GenTag Int16LE
  GenTagDecayVolEnv :: GenTag Int16LE
  GenTagSustainVolEnv :: GenTag Int16LE
  GenTagReleaseVolEnv :: GenTag Int16LE
  GenTagKeyToVolEnvHold :: GenTag Int16LE
  GenTagKeyToVolEnvDecay :: GenTag Int16LE
  GenTagInstIndex :: GenTag Word16LE
  GenTagKeyRange :: GenTag Range
  GenTagVelRange :: GenTag Range
  GenTagLoopStartAddressCoarseOffset :: GenTag Int16LE
  GenTagKey :: GenTag Word16LE
  GenTagVel :: GenTag Word16LE
  GenTagInitAtten :: GenTag Int16LE
  GenTagLoopEndAddressCoarseOffset :: GenTag Int16LE
  GenTagCoarseTune :: GenTag Int16LE
  GenTagFineTune :: GenTag Int16LE
  GenTagSampleIndex :: GenTag Word16LE
  GenTagSampleMode :: GenTag SampleMode
  GenTagScaleTuning :: GenTag Int16LE
  GenTagExclusiveClass :: GenTag Int16LE
  GenTagRootKey :: GenTag Word16LE
  GenTagReserved :: Word16LE -> GenTag Int16LE

deriving instance Eq (GenTag a)
deriving instance Ord (GenTag a)
deriving instance Show (GenTag a)

genTagRep :: GenTag a -> Word16LE
genTagRep = \case
   GenTagStartAddressOffset -> 0
   GenTagEndAddressOffset -> 1
   GenTagLoopStartAddressOffset -> 2
   GenTagLoopEndAddressOffset -> 3
   GenTagStartAddressCoarseOffset -> 4
   GenTagModLfoToPitch -> 5
   GenTagVibLfoToPitch -> 6
   GenTagModEnvToPitch -> 7
   GenTagInitFc -> 8
   GenTagInitQ -> 9
   GenTagModLfoToFc -> 10
   GenTagModEnvToFc -> 11
   GenTagEndAddressCoarseOffset -> 12
   GenTagModLfoToVol -> 13
   GenTagChorus -> 15
   GenTagReverb -> 16
   GenTagPan -> 17
   GenTagDelayModLfo -> 21
   GenTagFreqModLfo -> 22
   GenTagDelayVibLfo -> 23
   GenTagFreqVibLfo -> 24
   GenTagDelayModEnv -> 25
   GenTagAttackModEnv -> 26
   GenTagHoldModEnv -> 27
   GenTagDecayModEnv -> 28
   GenTagSustainModEnv -> 29
   GenTagReleaseModEnv -> 30
   GenTagKeyToModEnvHold -> 31
   GenTagKeyToModEnvDecay -> 32
   GenTagDelayVolEnv -> 33
   GenTagAttackVolEnv -> 34
   GenTagHoldVolEnv -> 35
   GenTagDecayVolEnv -> 36
   GenTagSustainVolEnv -> 37
   GenTagReleaseVolEnv -> 38
   GenTagKeyToVolEnvHold -> 39
   GenTagKeyToVolEnvDecay -> 40
   GenTagInstIndex -> 41
   GenTagKeyRange -> 43
   GenTagVelRange -> 44
   GenTagLoopStartAddressCoarseOffset -> 45
   GenTagKey -> 46
   GenTagVel -> 47
   GenTagInitAtten -> 48
   GenTagLoopEndAddressCoarseOffset -> 50
   GenTagCoarseTune -> 51
   GenTagFineTune -> 52
   GenTagSampleIndex -> 53
   GenTagSampleMode -> 54
   GenTagScaleTuning -> 56
   GenTagExclusiveClass -> 57
   GenTagRootKey -> 58
   GenTagReserved t -> t

data GenPair a = GenPair
  { genPairTag :: !(GenTag a)
  , genPairVal :: !a
  } deriving stock (Eq, Ord, Show)

putGenPair :: Binary a => GenPair a -> Put
putGenPair (GenPair tag val) = put (genTagRep tag) *> put val

data Gen =
    GenInt !(GenPair Int16LE)
  | GenWord !(GenPair Word16LE)
  | GenRange !(GenPair Range)
  | GenSampleMode !(GenPair SampleMode)
  deriving stock (Eq, Ord, Show)
  deriving (ByteSized) via (ViaStaticByteSized Gen)

getGenInt :: GenTag Int16LE -> Get Gen
getGenInt tag = fmap (GenInt . GenPair tag) get

getGenWord :: GenTag Word16LE -> Get Gen
getGenWord tag = fmap (GenWord . GenPair tag) get

getGenRange :: GenTag Range -> Get Gen
getGenRange tag = fmap (GenRange . GenPair tag) get

getGenSampleMode :: GenTag SampleMode -> Get Gen
getGenSampleMode tag = fmap (GenSampleMode . GenPair tag) get

instance StaticByteSized Gen where
  staticByteSize _ = 4

instance Binary Gen where
  get = do
    tag <- get
    case tag of
      0 -> getGenInt GenTagStartAddressOffset
      1 -> getGenInt GenTagEndAddressOffset
      2 -> getGenInt GenTagLoopStartAddressOffset
      3 -> getGenInt GenTagLoopEndAddressOffset
      4 -> getGenInt GenTagStartAddressCoarseOffset
      5 -> getGenInt GenTagModLfoToPitch
      6 -> getGenInt GenTagVibLfoToPitch
      7 -> getGenInt GenTagModEnvToPitch
      8 -> getGenInt GenTagInitFc
      9 -> getGenInt GenTagInitQ
      10 -> getGenInt GenTagModLfoToFc
      11 -> getGenInt GenTagModEnvToFc
      12 -> getGenInt GenTagEndAddressCoarseOffset
      13 -> getGenInt GenTagModLfoToVol
      15 -> getGenInt GenTagChorus
      16 -> getGenInt GenTagReverb
      17 -> getGenInt GenTagPan
      21 -> getGenInt GenTagDelayModLfo
      22 -> getGenInt GenTagFreqModLfo
      23 -> getGenInt GenTagDelayVibLfo
      24 -> getGenInt GenTagFreqVibLfo
      25 -> getGenInt GenTagDelayModEnv
      26 -> getGenInt GenTagAttackModEnv
      27 -> getGenInt GenTagHoldModEnv
      28 -> getGenInt GenTagDecayModEnv
      29 -> getGenInt GenTagSustainModEnv
      30 -> getGenInt GenTagReleaseModEnv
      31 -> getGenInt GenTagKeyToModEnvHold
      32 -> getGenInt GenTagKeyToModEnvDecay
      33 -> getGenInt GenTagDelayVolEnv
      34 -> getGenInt GenTagAttackVolEnv
      35 -> getGenInt GenTagHoldVolEnv
      36 -> getGenInt GenTagDecayVolEnv
      37 -> getGenInt GenTagSustainVolEnv
      38 -> getGenInt GenTagReleaseVolEnv
      39 -> getGenInt GenTagKeyToVolEnvHold
      40 -> getGenInt GenTagKeyToVolEnvDecay
      41 -> getGenWord GenTagInstIndex
      43 -> getGenRange GenTagKeyRange
      44 -> getGenRange GenTagVelRange
      45 -> getGenInt GenTagLoopStartAddressCoarseOffset
      46 -> getGenWord GenTagKey
      47 -> getGenWord GenTagVel
      48 -> getGenInt GenTagInitAtten
      50 -> getGenInt GenTagLoopEndAddressCoarseOffset
      51 -> getGenInt GenTagCoarseTune
      52 -> getGenInt GenTagFineTune
      53 -> getGenWord GenTagSampleIndex
      54 -> getGenSampleMode GenTagSampleMode
      56 -> getGenInt GenTagScaleTuning
      57 -> getGenInt GenTagExclusiveClass
      58 -> getGenWord GenTagRootKey
      _ -> getGenInt (GenTagReserved tag)
  put = \case
    GenInt gp -> putGenPair gp
    GenWord gp -> putGenPair gp
    GenRange gp -> putGenPair gp
    GenSampleMode gp -> putGenPair gp

defGenVal :: GenTag a -> a
defGenVal = \case
  GenTagStartAddressOffset -> 0
  GenTagEndAddressOffset -> 0
  GenTagLoopStartAddressOffset -> 0
  GenTagLoopEndAddressOffset -> 0
  GenTagStartAddressCoarseOffset -> 0
  GenTagModLfoToPitch -> 0
  GenTagVibLfoToPitch -> 0
  GenTagModEnvToPitch -> 0
  GenTagInitFc -> 13500
  GenTagInitQ -> 0
  GenTagModLfoToFc -> 0
  GenTagModEnvToFc -> 0
  GenTagEndAddressCoarseOffset -> 0
  GenTagModLfoToVol -> 0
  GenTagChorus -> 0
  GenTagReverb -> 0
  GenTagPan -> 0
  GenTagDelayModLfo -> -12000
  GenTagFreqModLfo -> 0
  GenTagDelayVibLfo -> -12000
  GenTagFreqVibLfo -> 0
  GenTagDelayModEnv -> -12000
  GenTagAttackModEnv -> -12000
  GenTagHoldModEnv -> -12000
  GenTagDecayModEnv -> -12000
  GenTagSustainModEnv -> 0
  GenTagReleaseModEnv -> -12000
  GenTagKeyToModEnvHold -> 0
  GenTagKeyToModEnvDecay -> 0
  GenTagDelayVolEnv -> -12000
  GenTagAttackVolEnv -> -12000
  GenTagHoldVolEnv -> -12000
  GenTagDecayVolEnv -> -12000
  GenTagSustainVolEnv -> 0
  GenTagReleaseVolEnv -> -12000
  GenTagKeyToVolEnvHold -> 0
  GenTagKeyToVolEnvDecay -> 0
  GenTagInstIndex -> 0
  GenTagKeyRange -> Range 0 127
  GenTagVelRange -> Range 0 127
  GenTagLoopStartAddressCoarseOffset -> 0
  GenTagKey -> -1
  GenTagVel -> -1
  GenTagInitAtten -> 0
  GenTagLoopEndAddressCoarseOffset -> 0
  GenTagCoarseTune -> 0
  GenTagFineTune -> 0
  GenTagSampleIndex -> 0
  GenTagSampleMode -> SampleModeNoLoop 0
  GenTagScaleTuning -> 100
  GenTagExclusiveClass -> 0
  GenTagRootKey -> -1
  GenTagReserved _ -> 0

genTagTyRep :: Typeable a => GenTag a -> TypeRep a
genTagTyRep _ = typeRep

genTagTest :: (Typeable a, Typeable b) => GenTag a -> GenTag b -> Maybe (a :~: b)
genTagTest t s = case testEquality (genTagTyRep t) (genTagTyRep s) of
  Nothing -> Nothing
  Just Refl -> if t == s then Just Refl else Nothing

lookupGenVal :: Typeable a => GenTag a -> Seq Gen -> Maybe a
lookupGenVal t = go where
  go = \case
    Empty -> Nothing
    gp :<| rest ->
      case gp of
        GenInt (GenPair s v) ->
          case genTagTest t s of
            Just Refl -> Just v
            Nothing -> go rest
        GenWord (GenPair s v) ->
          case genTagTest t s of
            Just Refl -> Just v
            Nothing -> go rest
        GenRange (GenPair s v) ->
          case genTagTest t s of
            Just Refl -> Just v
            Nothing -> go rest
        GenSampleMode (GenPair s v) ->
          case genTagTest t s of
            Just Refl -> Just v
            Nothing -> go rest
