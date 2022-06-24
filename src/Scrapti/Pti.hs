{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scrapti.Pti where

import Data.Default (Default (..))
import Data.Int (Int8)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Scrapti.Binary (Binary (..), BoolByte (..), FixedBytes, FixedText, FixedVec, FloatLE, Int16LE, Word16LE,
                       Word32LE)
import Scrapti.Classes (BinaryRep (..), Equiv (..), Pair (..), ViaBinaryRep (..), ViaBoundedEnum (..), ViaEquiv (..))
import Scrapti.Wav (Wav)

data WavetableWindowSize =
    WWS32
  | WWS64
  | WWS128
  | WWS256
  | WWS1024
  | WWS2048
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving Binary via (ViaBinaryRep WavetableWindowSize)

instance Default WavetableWindowSize where
  def = WWS2048

instance BinaryRep Word16LE WavetableWindowSize where
  parse = \case
    32 -> Right WWS32
    64 -> Right WWS64
    128 -> Right WWS128
    256 -> Right WWS256
    1024 -> Right WWS1024
    2048 -> Right WWS2048
    other -> Left ("invalid wavetable window size: " ++ show other)
  rep = \case
    WWS32 -> 32
    WWS64 -> 64
    WWS128 -> 128
    WWS256 -> 256
    WWS1024 -> 1024
    WWS2048 -> 2048

data SamplePlayback =
    SPOneShot
  | SPForwardLoop
  | SPBackwardLoop
  | SPPingPongLoop
  | SPSlice
  | SPBeatSlice
  | SPWavetable
  | SPGranular
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 SamplePlayback)
  deriving Binary via (ViaBinaryRep SamplePlayback)

instance Default SamplePlayback where
  def = SPOneShot

data Preamble = Preamble
  { preIsWavetable :: !BoolByte
  , preName :: !(FixedText 30)
  , preSampleLength :: !Word32LE
  , preWavetableWindowSize :: !WavetableWindowSize
  , preWavetableTotalPositions :: !Word16LE
  , preSamplePlayback :: !SamplePlayback
  , prePlaybackStart :: !Word16LE
  , preLoopStart :: !Word16LE
  , preLoopEnd :: !Word16LE
  , prePlaybackEnd :: !Word16LE
  , preWavetablePosition :: !Word16LE
  } deriving stock (Eq, Show)

instance Default Preamble where
  def = Preamble (BoolByte False) "" 0 def 0 def 0 1 65534 65535 0

data AuxPreamble = AuxPreamble
  { auxPre0To19 :: !(FixedBytes 20)
  , auxPre52To59 :: !(FixedBytes 8)
  , auxPre66To67 :: !Word16LE
  , auxPre70To75 :: !(FixedBytes 6)
  , auxPre77 :: !Word8
  , auxPre86To87 :: !Word16LE
  , auxPre90To91 :: !Word16LE
  } deriving stock (Eq, Show)

instance Default AuxPreamble where
  def = AuxPreamble def def def def def def def

data MixPreamble = MixPreamble
  { auxPre0To19 :: !(FixedBytes 20)
  -- ^ 0-19
  , preIsWavetable :: !BoolByte
  -- 20
  , preName :: !(FixedText 30)
  -- 21-51
  , auxPre52To59 :: !(FixedBytes 8)
  -- 52-59
  , preSampleLength :: !Word32LE
  -- 60-63
  , preWavetableWindowSize :: !WavetableWindowSize
  -- 64-65
  , auxPre66To67 :: !Word16LE
  -- 66-67
  , preWavetableTotalPositions :: !Word16LE
  -- 68-69
  , auxPre70To75 :: !(FixedBytes 6)
  -- 70-75
  , preSamplePlayback :: !SamplePlayback
  -- 76
  , auxPre77 :: !Word8
  -- 77
  , prePlaybackStart :: !Word16LE
  -- 78-79
  , preLoopStart :: !Word16LE
  -- 80-81
  , preLoopEnd :: !Word16LE
  -- 82-83
  , prePlaybackEnd :: !Word16LE
  -- 84-85
  , auxPre86To87 :: !Word16LE
  -- 86-87
  , preWavetablePosition :: !Word16LE
  -- 88-89
  , auxPre90To91 :: !Word16LE
  -- 90-91
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

newtype PairPreamble = PairPreamble { unPairPreamble :: Pair AuxPreamble Preamble }
  deriving stock (Show)
  deriving newtype (Eq, Default)
  deriving (Binary) via (ViaEquiv PairPreamble)

instance Equiv PairPreamble (Pair AuxPreamble Preamble) where
  equivFwd = unPairPreamble
  equivBwd = PairPreamble

instance Equiv MixPreamble PairPreamble where
  equivFwd (MixPreamble {..}) = PairPreamble (Pair (AuxPreamble {..}) (Preamble {..}))
  equivBwd (PairPreamble (Pair (AuxPreamble {..}) (Preamble {..}))) = MixPreamble {..}

data AutoEnvelope = AutoEnvelope
  { aeAmount :: !FloatLE
  , aeAttack :: !Word16LE
  , aeDecay :: !Word16LE
  , aeSustain :: !FloatLE
  , aeRelease :: !Word16LE
  } deriving stock (Eq, Show)

instance Default AutoEnvelope where
  def = AutoEnvelope 1.0 0 0 1.0 1000

data AuxAutoEnvelope = AuxAutoEnvelope
  { aae96To97 :: !Word16LE
  , aae100To101 :: !Word16LE
  } deriving stock (Eq, Show)

instance Default AuxAutoEnvelope where
  def = AuxAutoEnvelope 0 0

data MixAutoEnvelope = MixAutoEnvelope
  { aeAmount :: !FloatLE
  -- 92-95
  , aae96To97 :: !Word16LE
  -- 96-97
  , aeAttack :: !Word16LE
  -- 98-909
  , aae100To101 :: !Word16LE
  -- 100-101
  , aeDecay :: !Word16LE
  -- 102-103
  , aeSustain :: !FloatLE
  -- 104-107
  , aeRelease :: !Word16LE
  -- 108-109
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

newtype PairAutoEnvelope = PairAutoEnvelope { unPairAutoEnvelope :: Pair AuxAutoEnvelope AutoEnvelope }
  deriving stock (Show)
  deriving newtype (Eq, Default)
  deriving (Binary) via (ViaEquiv PairAutoEnvelope)

instance Equiv PairAutoEnvelope (Pair AuxAutoEnvelope AutoEnvelope) where
  equivFwd = unPairAutoEnvelope
  equivBwd = PairAutoEnvelope

instance Equiv MixAutoEnvelope PairAutoEnvelope where
  equivFwd (MixAutoEnvelope {..}) = PairAutoEnvelope (Pair (AuxAutoEnvelope {..}) (AutoEnvelope {..}))
  equivBwd (PairAutoEnvelope (Pair (AuxAutoEnvelope {..}) (AutoEnvelope {..}))) = MixAutoEnvelope {..}

data AutoType =
    ATOff
  | ATEnvelope
  | ATLfo
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving Binary via (ViaBinaryRep AutoType)

instance Default AutoType where
  def = ATEnvelope

instance BinaryRep Word16LE AutoType where
  parse = \case
    0x0000 -> Right ATOff
    0x0001 -> Right ATEnvelope
    0x0101 -> Right ATLfo
    other -> Left ("invalid automation type: " ++ show other)
  rep = \case
    ATOff -> 0x0000
    ATEnvelope -> 0x0001
    ATLfo -> 0x0101

data Auto = Auto
  { autoEnvelope :: !AutoEnvelope
  , autoType :: !AutoType
  } deriving stock (Eq, Show)

instance Default Auto where
  def = Auto def def

newtype PairAuto = PairAuto { unPairAuto :: Pair AuxAutoEnvelope Auto }
  deriving stock (Show)
  deriving newtype (Eq, Default)

instance Equiv PairAuto (Pair AuxAutoEnvelope Auto) where
  equivFwd = unPairAuto
  equivBwd = PairAuto

instance Binary PairAuto where
  get = do
    PairAutoEnvelope (Pair aae ae) <- get
    at <- get
    let !pair = Pair aae (Auto ae at)
    pure $! PairAuto pair
  put (PairAuto (Pair aae (Auto ae at))) = do
    put (PairAutoEnvelope (Pair aae ae))
    put at

data LfoType =
    LTRevSaw
  | LTSaw
  | LTTriangle
  | LTSquare
  | LTRandom
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 LfoType)
  deriving (Binary) via (ViaBinaryRep LfoType)

instance Default LfoType where
  def = LTTriangle

data LfoSteps =
    LS24
  | LS16
  | LS12
  | LS8
  | LS6
  | LS4
  | LS3
  | LS2
  | LS3Over2
  | LS1
  | LS3Over4
  | LS1Over2
  | LS3Over8
  | LS1Over3
  | LS1Over4
  | LS3Over16
  | LS1Over6
  | LS1Over8
  | LS1Over12
  | LS1Over16
  | LS1Over24
  | LS1Over32
  | LS1Over48
  | LS1Over64
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 LfoSteps)
  deriving Binary via (ViaBinaryRep LfoSteps)

instance Default LfoSteps where
  def = LS24

data Lfo = Lfo
  { lfoType :: !LfoType
  , lfoSteps :: !LfoSteps
  , lfoAmount :: !FloatLE
  } deriving stock (Eq, Show)

instance Default Lfo where
  def = Lfo def def 0.5

newtype AuxLfo = AuxLfo
  { auxLfo214To215 :: Word16LE
  } deriving stock (Show)
    deriving newtype (Eq)

instance Default AuxLfo where
  def = AuxLfo 0

data MixLfo = MixLfo
  { lfoType :: !LfoType
  -- 212
  , lfoSteps :: !LfoSteps
  -- 213
  , auxLfo214To215 :: Word16LE
  -- 214-215
  , lfoAmount :: !FloatLE
  -- 216-219
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

newtype PairLfo = PairLfo { unPairLfo :: Pair AuxLfo Lfo }
  deriving stock (Show)
  deriving newtype (Eq, Default)
  deriving (Binary) via (ViaEquiv PairLfo)

instance Equiv PairLfo (Pair AuxLfo Lfo) where
  equivFwd = unPairLfo
  equivBwd = PairLfo

instance Equiv MixLfo PairLfo where
  equivFwd (MixLfo {..}) = PairLfo (Pair (AuxLfo {..}) (Lfo {..}))
  equivBwd (PairLfo (Pair (AuxLfo {..}) (Lfo {..}))) = MixLfo {..}

data FilterType =
    FTDisabled
  | FTLowPass
  | FTHighPass
  | FTBandPass
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving Binary via (ViaBinaryRep FilterType)

instance Default FilterType where
  def = FTDisabled

instance BinaryRep Word16LE FilterType where
  parse = \case
    0x0000 -> pure FTDisabled
    0x0001 -> pure FTLowPass
    0x0101 -> pure FTHighPass
    0x0201 -> pure FTBandPass
    other -> Left ("invalid filter type: " ++ show other)
  rep = \case
    FTDisabled -> 0x0000
    FTLowPass -> 0x0001
    FTHighPass -> 0x0101
    FTBandPass -> 0x0201

data Filter = Filter
  { filtCutoff :: !Float
  , filtResonance :: !Float
  , filtType :: !FilterType
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

instance Default Filter where
  def = Filter 1.0 0.0 def

data InstParams = InstParams
  { ipTune :: !Int8
  , ipFineTune :: !Int8
  , ipVolume :: !Word8
  , ipPanning :: !Word8
  , ipDelaySend :: !Word8
  } deriving stock (Eq, Show)

instance Default InstParams where
  def = InstParams 0 0 50 50 0

data AuxInstParams = AuxInstParams
  { aip273To275 :: !Word16LE
  , aip277 :: !Word8
  , aip279 :: !Word8
  } deriving stock (Eq, Show)

instance Default AuxInstParams where
  def = AuxInstParams 0 0 0

data MixInstParams = MixInstParams
  { ipTune :: !Int8
  -- 270
  , ipFineTune :: !Int8
  -- 271
  , ipVolume :: !Word8
  -- 272
  , aip273To275 :: !Word16LE
  -- 273-275
  , ipPanning :: !Word8
  -- 276
  , aip277 :: !Word8
  -- 277
  , ipDelaySend :: !Word8
  -- 278
  , aip279 :: !Word8
  -- 279
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

newtype PairInstParams = PairInstParams { unPairInstParams :: Pair AuxInstParams InstParams }
  deriving stock (Show)
  deriving newtype (Eq, Default)
  deriving (Binary) via (ViaEquiv PairInstParams)

instance Equiv PairInstParams (Pair AuxInstParams InstParams) where
  equivFwd = unPairInstParams
  equivBwd = PairInstParams

instance Equiv MixInstParams PairInstParams where
  equivFwd (MixInstParams {..}) = PairInstParams (Pair (AuxInstParams {..}) (InstParams {..}))
  equivBwd (PairInstParams (Pair (AuxInstParams {..}) (InstParams {..}))) = MixInstParams {..}

numSlices :: Int
numSlices = 48

data Slices = Slices
  { slicesAdjust :: !(FixedVec 48 Word16LE)
  , slicesNumber :: !Word8
  , slicesActive :: !Word8
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

instance Default Slices where
  def = Slices def 0 0

data GranularShape =
    GSSquare
  | GSTriangle
  | GSGauss
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 GranularShape)
  deriving Binary via (ViaBinaryRep GranularShape)

instance Default GranularShape where
  def = GSSquare

data GranularLoopMode =
    GLMForward
  | GLMBackward
  | GLMPingPong
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 GranularLoopMode)
  deriving Binary via (ViaBinaryRep GranularLoopMode)

instance Default GranularLoopMode where
  def = GLMForward

data Granular = Granular
  { granLen :: !Word16LE
  , granPos :: !Word16LE
  , granShape :: !GranularShape
  , granLoopMode :: !GranularLoopMode
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

instance Default Granular where
  def = Granular 441 0 def def

data Effects = Effects
  { effReverbSend :: !Word8
  , effOverdrive :: !Word8
  , effBitDepth :: !Word8
  } deriving stock (Eq, Show)

instance Default Effects where
  def = Effects 0 0 16

data AuxEffects = AuxEffects
  { auxEff387 :: !Word8
  , auxEff388To391 :: !Word32LE
  } deriving stock (Eq, Show)

instance Default AuxEffects where
  def = AuxEffects 0 0

data MixEffects = MixEffects
  { effReverbSend :: !Word8
  -- 384
  , effOverdrive :: !Word8
  -- 385
  , effBitDepth :: !Word8
  -- 386
  , auxEff387 :: !Word8
  -- 387
  , auxEff388To391 :: !Word32LE
  -- 388-391
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

newtype PairEffects = PairEffects { unPairEffects :: Pair AuxEffects Effects }
  deriving stock (Show)
  deriving newtype (Eq, Default)
  deriving (Binary) via (ViaEquiv PairEffects)

instance Equiv PairEffects (Pair AuxEffects Effects) where
  equivFwd = unPairEffects
  equivBwd = PairEffects

instance Equiv MixEffects PairEffects where
  equivFwd (MixEffects {..}) = PairEffects (Pair (AuxEffects {..}) (Effects {..}))
  equivBwd (PairEffects (Pair (AuxEffects {..}) (Effects {..}))) = MixEffects {..}

data Block a = Block
  { blockVolume :: !a
  , blockPanning :: !a
  , blockCutoff :: !a
  , blockWavetablePosition :: !a
  , blockGranularPosition :: !a
  , blockFinetune :: !a
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

instance Default a => Default (Block a) where
  def = let a = def in Block a a a a a a

splitBlock :: Equiv a (Pair x y) => Block a -> Pair (Block x) (Block y)
splitBlock (Block a b c d e f) =
  let Pair a1 a2 = equivFwd a
      Pair b1 b2 = equivFwd b
      Pair c1 c2 = equivFwd c
      Pair d1 d2 = equivFwd d
      Pair e1 e2 = equivFwd e
      Pair f1 f2 = equivFwd f
      z1 = Block a1 b1 c1 d1 e1 f1
      z2 = Block a2 b2 c2 d2 e2 f2
  in Pair z1 z2

joinBlock :: Equiv a (Pair x y) => Pair (Block x) (Block y) -> Block a
joinBlock (Pair (Block a1 b1 c1 d1 e1 f1) (Block a2 b2 c2 d2 e2 f2)) =
  Block
    (equivBwd (Pair a1 a2))
    (equivBwd (Pair b1 b2))
    (equivBwd (Pair c1 c2))
    (equivBwd (Pair d1 d2))
    (equivBwd (Pair e1 e2))
    (equivBwd (Pair f1 f2))

data Header = Header
  { headPreamble :: !Preamble
  , headAutoBlock :: !(Block Auto)
  , headLfoBlock :: !(Block Lfo)
  , headFilter :: !Filter
  , headInstParams :: !InstParams
  , headSlices :: !Slices
  , headGranular :: !Granular
  , headEffects :: !Effects
  } deriving stock (Eq, Show)

instance Default Header where
  def = Header def def def def def def def def

data AuxHeader = AuxHeader
  { ahPreamble :: !AuxPreamble
  , ahAutoBlock :: !(Block AuxAutoEnvelope)
  , ahLfoBlock :: !(Block AuxLfo)
  , ahInstParams :: !AuxInstParams
  , ahEffects :: !AuxEffects
  } deriving stock (Eq, Show)

instance Default AuxHeader where
  def = AuxHeader def def def def def

data MixHeader = MixHeader
  { mhPreamble :: !PairPreamble
  , mhAutoBlock :: !(Block PairAuto)
  , mhLfoBlock :: !(Block PairLfo)
  , mhFilter :: !Filter
  , mhInstParams :: !PairInstParams
  , mhSlices :: !Slices
  , mhGranular :: !Granular
  , mhEffects :: !PairEffects
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Binary)

newtype PairHeader = PairHeader { unPairHeader :: Pair AuxHeader Header }
  deriving stock (Show)
  deriving newtype (Eq, Default)
  deriving (Binary) via (ViaEquiv PairHeader)

instance Equiv PairHeader (Pair AuxHeader Header) where
  equivFwd = unPairHeader
  equivBwd = PairHeader

instance Equiv MixHeader PairHeader where
  equivFwd (MixHeader {..}) =
    let Pair ahPreamble headPreamble = equivFwd mhPreamble
        Pair ahAutoBlock headAutoBlock = splitBlock mhAutoBlock
        Pair ahLfoBlock headLfoBlock = splitBlock mhLfoBlock
        headFilter = mhFilter
        Pair ahInstParams headInstParams = equivFwd mhInstParams
        headSlices = mhSlices
        headGranular = mhGranular
        Pair ahEffects headEffects = equivFwd mhEffects
        ahd = AuxHeader {..}
        hd = Header {..}
    in PairHeader (Pair ahd hd)
  equivBwd (PairHeader (Pair (AuxHeader {..}) (Header {..}))) =
    let mhPreamble = equivBwd (Pair ahPreamble headPreamble)
        mhAutoBlock = joinBlock (Pair ahAutoBlock headAutoBlock)
        mhLfoBlock = joinBlock (Pair ahLfoBlock headLfoBlock)
        mhFilter = headFilter
        mhInstParams = equivBwd (Pair ahInstParams headInstParams)
        mhSlices = headSlices
        mhGranular = headGranular
        mhEffects = equivBwd (Pair ahEffects headEffects)
    in MixHeader {..}

data Pti = Pti
  { ptiHeader :: !Header
  , ptiWav :: !(Wav Int16LE)
  } deriving stock (Eq, Show)

instance Default Pti where
  def = Pti def def

newtype PairPti = PairPti { unPairPti :: Pair AuxHeader Pti }
  deriving stock (Show)
  deriving newtype (Eq, Default)

instance Equiv PairPti (Pair AuxHeader Pti) where
  equivFwd = unPairPti
  equivBwd = PairPti

instance Binary PairPti where
  get = do
    PairHeader (Pair ahd hd) <- get
    wav <- get
    let !pair = Pair ahd (Pti hd wav)
    pure $! PairPti pair
  put (PairPti (Pair ahd (Pti hd wav))) = do
    put (PairHeader (Pair ahd hd))
    put wav
