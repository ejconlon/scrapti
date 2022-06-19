module Scrapti.Sfont where

import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word32, Word16)
import Scrapti.Wav (WavData)
import Data.Int (Int16, Int8)

data Sfont a = Sfont
  { sfontInfos :: !(Seq Info)
  , sfontSdta :: !Sdta
  , sfontPdta :: !Pdta
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
  | InfoReservedInfo !Text !Word16 !ByteString
  deriving stock (Eq, Show)

data Sdta = Sdta
  { sdtaHighBits :: !(WavData Int16)
  , sdtaLowBits :: !(Maybe (WavData Int8))
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
