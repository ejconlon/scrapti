{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Sfont
  ( ListChunk (..)
  , Sfont (..)
  , InfoChunk (..)
  , PdtaChunk (..)
  , Info (..)
  , SdtaChunk (..)
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
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import Data.Int (Int8)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8)
import GHC.Generics (Generic)
import Scrapti.Binary (Binary (..), BinaryParser (..), ByteLength, ByteSized (..), FixedText, Int16LE, ParseM,
                       StaticByteSized (..), TermText, ViaStaticByteSized (..), Word16LE, Word32LE, getWithoutSize,
                       parseBound, parseRemaining, parseRepeated, parseSkip, parseVec, putByteString, putSeq, putVec)
import Scrapti.Riff (Label, StaticLabel (..), chunkHeaderSize, labelRiff, parseChunkSize, parseExpectLabel,
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

sizePhdr, sizeBag, sizeMod, sizeGen, sizeInst, sizeShdr :: ByteLength
sizePhdr = 38
sizeBag = 4
sizeMod = 10
sizeGen = 4
sizeInst = 22
sizeShdr = 46

newtype SampleCount = SampleCount { unSampleCount :: Word32LE }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Binary)

type ShortText = FixedText 20

listChunkHeaderSize :: ByteLength
listChunkHeaderSize = chunkHeaderSize + 4

newtype ListChunk a = ListChunk { listChunkElems :: Seq a }
  deriving stock (Show)
  deriving newtype (Eq)

instance (StaticLabel a, BinaryParser a) => Binary (ListChunk a) where
  get = getWithoutSize
  put (ListChunk elems) = do
    put labelList
    let !chunkSize = byteSize elems + 4
    putChunkSize chunkSize
    let !label = staticLabel (Proxy :: Proxy a)
    put label
    putSeq elems

instance ByteSized a => ByteSized (ListChunk a) where
  byteSize (ListChunk elems) = listChunkHeaderSize + byteSize elems

instance (StaticLabel a, BinaryParser a) => BinaryParser (ListChunk a) where
  parseWithoutSize = do
    parseExpectLabel labelList
    chunkSize <- parseChunkSize
    parseBound chunkSize $ do
      let !label = staticLabel (Proxy :: Proxy a)
      parseExpectLabel label
      elems <- parseRepeated
      pure $! ListChunk elems

data Sfont = Sfont
  { sfontInfo :: !InfoChunk
  , sfontSdta :: !SdtaChunk
  , sfontPdta :: !PdtaChunk
  } deriving stock (Eq, Show)

instance Binary Sfont where
  get = getWithoutSize
  put sfont@(Sfont info sdta pdta) = do
    put labelRiff
    let !chunkSize = byteSize sfont + 4
    putChunkSize chunkSize
    put labelSfbk
    put info
    put sdta
    put pdta

instance ByteSized Sfont where
  byteSize (Sfont info sdta pdta) = chunkHeaderSize + byteSize info + byteSize sdta + byteSize pdta

instance BinaryParser Sfont where
  parseWithoutSize = do
    parseExpectLabel labelRiff
    chunkSize <- parseChunkSize
    parseBound chunkSize $ do
      parseExpectLabel labelSfbk
      info <- parseWithoutSize
      sdta <- parseWithoutSize
      pdta <- parseWithoutSize
      pure $! Sfont info sdta pdta

newtype InfoChunk = InfoChunk { unInfoChunk :: ListChunk Info }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, Binary)

instance BinaryParser InfoChunk

newtype PdtaChunk = PdtaChunk { unPdtaChunk :: ListChunk PdtaBlock }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, Binary)

instance BinaryParser PdtaChunk

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

instance StaticLabel Info where
  staticLabel = const labelInfo

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
  get = getWithoutSize
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
    InfoReserved _ bs -> fromIntegral (BS.length bs)

instance BinaryParser Info where
  parseWithoutSize = do
    label <- parseWithoutSize
    chunkSize <- parseChunkSize
    parseBound chunkSize $
      if
        | label == labelIfil -> do
          unless (chunkSize == 4) (fail "bad ifil chunk size")
          w1 <- parseWithoutSize
          w2 <- parseWithoutSize
          pure $! InfoVersion w1 w2
        | label == labelIver -> do
          unless (chunkSize == 4) (fail "bad iver chunk size")
          w1 <- parseWithoutSize
          w2 <- parseWithoutSize
          pure $! InfoRomVersion w1 w2
        | label == labelIsng -> fmap InfoTargetSoundEngine parseWithoutSize
        | label == labelInam -> fmap InfoBankName parseWithoutSize
        | label == labelIrom -> fmap InfoRomName parseWithoutSize
        | label == labelIcrd -> fmap InfoCreationDate parseWithoutSize
        | label == labelIeng -> fmap InfoAuthors parseWithoutSize
        | label == labelIprd -> fmap InfoIntendedProduct parseWithoutSize
        | label == labelIcop -> fmap InfoCopyrightMessage parseWithoutSize
        | label == labelIcmt -> fmap InfoComments parseWithoutSize
        | label == labelIsft -> fmap InfoUsedTools parseWithoutSize
        | otherwise -> do
          bs <- parseRemaining
          pure $! InfoReserved label bs

data SdtaChunk = SdtaChunk
  { sdtaHighBits :: !(VP.Vector Int16LE)
  , sdtaLowBits :: !(Maybe (VP.Vector Word8))
  } deriving stock (Eq, Show)

instance ByteSized SdtaChunk where
  byteSize (SdtaChunk high mlow) = sizeHigh + sizeLow where
    sizeHigh = 12 + 2 * fromIntegral (VP.length high)
    sizeLow = maybe 0 (\low -> 8 + fromIntegral (VP.length low)) mlow

instance Binary SdtaChunk where
  get = getWithoutSize
  put sdta@(SdtaChunk highBits mayLowBits) = do
    put labelList
    putChunkSize (byteSize sdta)
    put labelSdta
    put labelSmpl
    putChunkSize (fromIntegral (VP.length highBits * 2))
    putVec highBits
    case mayLowBits of
      Nothing -> pure ()
      Just lowBits -> do
        put labelSm24
        putChunkSize (fromIntegral (VP.length lowBits))
        putVec lowBits

parseHighBits :: SampleCount -> ParseM (VP.Vector Int16LE)
parseHighBits numSamples = parseVec Proxy (fromIntegral numSamples)

parseLowBits :: SampleCount -> ParseM (VP.Vector Word8)
parseLowBits numSamples = parseVec Proxy (fromIntegral numSamples)

instance BinaryParser SdtaChunk where
  parseWithoutSize = do
    parseExpectLabel labelList
    chunkSize <- parseChunkSize
    parseBound chunkSize $ do
      parseExpectLabel labelSdta
      parseExpectLabel labelSmpl
      highSize <- parseChunkSize
      let !numSamples = div (fromIntegral highSize) 2
      highBits <- parseHighBits numSamples
      let !numExtra = chunkSize - highSize - 12
      if
        | numExtra > 0 -> do
          parseExpectLabel labelSm24
          lowSize <- parseChunkSize
          let !expectedSize = if even numSamples then numSamples else numSamples + 1
          unless (fromIntegral lowSize == expectedSize) (fail "invalid low sample size")
          lowBits <- parseLowBits numSamples
          unless (even numSamples) (parseSkip 1)
          pure $! SdtaChunk highBits (Just lowBits)
        | numExtra == 0 -> pure $! SdtaChunk highBits Nothing
        | otherwise -> fail "invalid sdata chunk/sample sizes"

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
  staticLabel = const labelPdta

instance ByteSized PdtaBlock where
  byteSize block = chunkHeaderSize + case block of
    PdtaBlockPhdr phdrs -> sizePhdr * fromIntegral (Seq.length phdrs)
    PdtaBlockBag _ bags -> sizeBag * fromIntegral (Seq.length bags)
    PdtaBlockMod _ mods -> sizeMod * fromIntegral (Seq.length mods)
    PdtaBlockGen _ gens -> sizeGen * fromIntegral (Seq.length gens)
    PdtaBlockInst insts -> sizeInst * fromIntegral (Seq.length insts)
    PdtaBlockShdr shdrs -> sizeShdr * fromIntegral (Seq.length shdrs)

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
  get = getWithoutSize
  put block = do
    let !label = whichLabelPdtaBlock block
    put label
    let !chunkSize = byteSize block - chunkHeaderSize
    putChunkSize chunkSize
    case block of
      PdtaBlockPhdr phdrs -> putSeq phdrs
      PdtaBlockBag _ bags -> putSeq bags
      PdtaBlockMod _ mods -> putSeq mods
      PdtaBlockGen _ gens -> putSeq gens
      PdtaBlockInst insts -> putSeq insts
      PdtaBlockShdr shdrs -> putSeq shdrs

instance BinaryParser PdtaBlock where
  parseWithoutSize = do
    label <- parseWithoutSize
    chunkSize <- parseChunkSize
    parseBound chunkSize $
      if
        | label == labelPhdr ->
          fmap PdtaBlockPhdr parseRepeated
        | label == labelPbag ->
          fmap (PdtaBlockBag PdtaCatPreset) parseRepeated
        | label == labelPmod ->
          fmap (PdtaBlockMod PdtaCatPreset) parseRepeated
        | label == labelPgen ->
          fmap (PdtaBlockGen PdtaCatPreset) parseRepeated
        | label == labelInst ->
          fmap PdtaBlockInst parseRepeated
        | label == labelIbag ->
          fmap (PdtaBlockBag PdtaCatInst) parseRepeated
        | label == labelImod ->
          fmap (PdtaBlockMod PdtaCatInst) parseRepeated
        | label == labelIgen ->
          fmap (PdtaBlockGen PdtaCatInst) parseRepeated
        | label == labelShdr ->
          fmap PdtaBlockShdr parseRepeated
        | otherwise ->
          fail ("unrecognized pdta elem: " ++ show label)

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
    deriving anyclass (Binary)
    deriving (ByteSized) via (ViaStaticByteSized Phdr)

instance StaticByteSized Phdr where
  staticByteSize = const sizePhdr

instance BinaryParser Phdr

data Bag = Bag
  { bagGenIndex :: !Word16LE
  , bagModIndex :: !Word16LE
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)
    deriving (ByteSized) via (ViaStaticByteSized Bag)

instance StaticByteSized Bag where
  staticByteSize = const sizeBag

instance BinaryParser Bag

-- | Modulator
data Mod = Mod
  { modSrcOper :: !Word16LE
  , modDestOper :: !Word16LE
  , modAmount :: !Int16LE
  , modAmtSrcOper :: !Word16LE
  , modTransOper :: !Word16LE
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)
    deriving (ByteSized) via (ViaStaticByteSized Mod)

instance StaticByteSized Mod where
  staticByteSize = const sizeMod

instance BinaryParser Mod

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
  staticByteSize = const sizeGen

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
  get = getWithoutSize
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

instance BinaryParser Gen where
  parseWithoutSize = do
    tag <- parseWithoutSize
    if
      | tag == 0 -> fmap GenStartAddressOffset parseWithoutSize
      | tag == 1 -> fmap GenEndAddressOffset parseWithoutSize
      | tag == 2 -> fmap GenLoopStartAddressOffset parseWithoutSize
      | tag == 3 -> fmap GenLoopEndAddressOffset parseWithoutSize
      | tag == 4 -> fmap GenStartAddressCoarseOffset parseWithoutSize
      | tag == 5 -> fmap GenModLfoToPitch parseWithoutSize
      | tag == 6 -> fmap GenVibLfoToPitch parseWithoutSize
      | tag == 7 -> fmap GenModEnvToPitch parseWithoutSize
      | tag == 8 -> fmap GenInitFc parseWithoutSize
      | tag == 9 -> fmap GenInitQ parseWithoutSize
      | tag == 10 -> fmap GenModLfoToFc parseWithoutSize
      | tag == 11 -> fmap GenModEnvToFc parseWithoutSize
      | tag == 12 -> fmap GenEndAddressCoarseOffset parseWithoutSize
      | tag == 13 -> fmap GenModLfoToVol parseWithoutSize
      | tag == 15 -> fmap GenChorus parseWithoutSize
      | tag == 16 -> fmap GenReverb parseWithoutSize
      | tag == 17 -> fmap GenPan parseWithoutSize
      | tag == 21 -> fmap GenDelayModLfo parseWithoutSize
      | tag == 22 -> fmap GenFreqModLfo parseWithoutSize
      | tag == 23 -> fmap GenDelayVibLfo parseWithoutSize
      | tag == 24 -> fmap GenFreqVibLfo parseWithoutSize
      | tag == 25 -> fmap GenDelayModEnv parseWithoutSize
      | tag == 26 -> fmap GenAttackModEnv parseWithoutSize
      | tag == 27 -> fmap GenHoldModEnv parseWithoutSize
      | tag == 28 -> fmap GenDecayModEnv parseWithoutSize
      | tag == 29 -> fmap GenSustainModEnv parseWithoutSize
      | tag == 30 -> fmap GenReleaseModEnv parseWithoutSize
      | tag == 31 -> fmap GenKeyToModEnvHold parseWithoutSize
      | tag == 32 -> fmap GenKeyToModEnvDecay parseWithoutSize
      | tag == 33 -> fmap GenDelayVolEnv parseWithoutSize
      | tag == 34 -> fmap GenAttackVolEnv parseWithoutSize
      | tag == 35 -> fmap GenHoldVolEnv parseWithoutSize
      | tag == 36 -> fmap GenDecayVolEnv parseWithoutSize
      | tag == 37 -> fmap GenSustainVolEnv parseWithoutSize
      | tag == 38 -> fmap GenReleaseVolEnv parseWithoutSize
      | tag == 39 -> fmap GenKeyToVolEnvHold parseWithoutSize
      | tag == 40 -> fmap GenKeyToVolEnvDecay parseWithoutSize
      | tag == 41 -> fmap GenInstIndex parseWithoutSize
      | tag == 43 -> do
        a <- parseWithoutSize
        b <- parseWithoutSize
        pure $! GenKeyRange a b
      | tag == 44 -> do
        a <- parseWithoutSize
        b <- parseWithoutSize
        pure $! GenVelRange a b
      | tag == 45 -> fmap GenLoopStartAddressCoarseOffset parseWithoutSize
      | tag == 46 -> fmap GenKey parseWithoutSize
      | tag == 47 -> fmap GenVel parseWithoutSize
      | tag == 48 -> fmap GenInitAtten parseWithoutSize
      | tag == 50 -> fmap GenLoopEndAddressCoarseOffset parseWithoutSize
      | tag == 51 -> fmap GenCoarseTune parseWithoutSize
      | tag == 52 -> fmap GenFineTune parseWithoutSize
      | tag == 53 -> fmap GenSampleIndex parseWithoutSize
      | tag == 54 -> do
        a <- parseWithoutSize
        let !sm = case a of
              1 -> SampleModeContLoop
              3 -> SampleModePressLoop
              _ -> SampleModeNoLoop a
        pure $! GenSampleMode sm
      | tag == 56 -> fmap GenScaleTuning parseWithoutSize
      | tag == 57 -> fmap GenExclusiveClass parseWithoutSize
      | tag == 58 -> fmap GenRootKey parseWithoutSize
      | otherwise -> do
        a <- parseWithoutSize
        pure $! GenReserved tag a

-- | Instrument
data Inst = Inst
  { instName :: !ShortText
  , instBagIndex :: !Word16LE
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)
    deriving (ByteSized) via (ViaStaticByteSized Inst)

instance StaticByteSized Inst where
  staticByteSize = const sizeInst

instance BinaryParser Inst

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
    deriving anyclass (Binary)
    deriving (ByteSized) via (ViaStaticByteSized Shdr)

instance StaticByteSized Shdr where
  staticByteSize = const sizeShdr

instance BinaryParser Shdr

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
