{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scrapti.Pti where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (Default (..))
import Data.Int (Int8)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8)
import Scrapti.Binary (Binary (..), BoolByte (..), DecodeT, FloatLE, Get, Int16LE, Word16LE, Word32LE, decodeGet,
                       getByteString, getFixedString, putByteString, putFixedString)
import Scrapti.Classes (BinaryRep (..), ViaBinaryRep (..), ViaBoundedEnum (..))
import Scrapti.Wav (Wav, decodeSpecificWav)

-- | Just a strict tuple
data Pair a b = Pair
  { pairFirst :: !a
  , pairSecond :: !b
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance (Default a, Default b) => Default (Pair a b) where
  def = Pair def def

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
  , preName :: !Text
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
  { auxPre0To19 :: !ByteString
  , auxPre52To59 :: !ByteString
  , auxPre66To67 :: !Word16LE
  , auxPre70To75 :: !ByteString
  , auxPre77 :: !Word8
  , auxPre86To87 :: !Word16LE
  , auxPre90To91 :: !Word16LE
  } deriving stock (Eq, Show)

instance Default AuxPreamble where
  def = AuxPreamble (BS.replicate 20 0) (BS.replicate 8 0) def (BS.replicate 6 0) def def def

newtype PairPreamble = PairPreamble { unPairPreamble :: Pair AuxPreamble Preamble }
  deriving stock (Show)
  deriving newtype (Eq, Default)

instance Binary PairPreamble where
  get = do
    -- 0-19
    auxPre0To19 <- getByteString 20
    -- 20
    preIsWavetable <- get
    -- 21-51
    preName <- getFixedString 30
    -- 52-59
    auxPre52To59 <- getByteString 8
    -- 60-63
    preSampleLength <- get
    -- 64-65
    preWavetableWindowSize <- get
    -- 66-67
    auxPre66To67 <- get
    -- 68-69
    preWavetableTotalPositions <- get
    -- 70-75
    auxPre70To75 <- getByteString 6
    -- 76
    preSamplePlayback <- get
    -- 77
    auxPre77 <- get
    -- 78-79
    prePlaybackStart <- get
    -- 80-81
    preLoopStart <- get
    -- 82-83
    preLoopEnd <- get
    -- 84-85
    prePlaybackEnd <- get
    -- 86-87
    auxPre86To87 <- get
    -- 88-89
    preWavetablePosition <- get
    -- 90-91
    auxPre90To91 <- get
    let !auxPre = AuxPreamble {..}
        !pre = Preamble {..}
        !pair = Pair auxPre pre
    pure $! PairPreamble pair
  put (PairPreamble (Pair (AuxPreamble {..}) (Preamble {..}))) = do
    -- 0-19
    putByteString auxPre0To19
    -- 20
    put preIsWavetable
    -- 21-51
    putFixedString 30 preName
    -- 52-59
    putByteString auxPre52To59
    -- 60-63
    put preSampleLength
    -- 64-65
    put preWavetableWindowSize
    -- 66-67
    put auxPre66To67
    -- 68-69
    put preWavetableTotalPositions
    -- 70-75
    putByteString auxPre70To75
    -- 76
    put preSamplePlayback
    -- 77
    put auxPre77
    -- 78-79
    put prePlaybackStart
    -- 80-81
    put preLoopStart
    -- 82-83
    put preLoopEnd
    -- 84-85
    put prePlaybackEnd
    -- 86-87
    put auxPre86To87
    -- 88-89
    put preWavetablePosition
    -- 90-91
    put auxPre90To91

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

newtype PairAutoEnvelope = PairAutoEnvelope { unPairAutoEnvelope :: Pair AuxAutoEnvelope AutoEnvelope }
  deriving stock (Show)
  deriving newtype (Eq, Default)

instance Binary PairAutoEnvelope where
  get = do
    -- 92-95
    aeAmount <- get
    -- 96-97
    aae96To97 <- get
    -- 98-99
    aeAttack <- get
    -- 100-101
    aae100To101 <- get
    -- 102-103
    aeDecay <- get
    -- 104-107
    aeSustain <- get
    -- 108-109
    aeRelease <- get
    let !aae = AuxAutoEnvelope {..}
        !ae = AutoEnvelope {..}
        !pair = Pair aae ae
    pure $! PairAutoEnvelope pair
  put (PairAutoEnvelope (Pair (AuxAutoEnvelope {..}) (AutoEnvelope {..}))) = do
    -- 92-95
    put aeAmount
    -- 96-97
    put aae96To97
    -- 98-99
    put aeAttack
    -- 100-101
    put aae100To101
    -- 102-103
    put aeDecay
    -- 104-107
    put aeSustain
    -- 108-109
    put aeRelease

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

newtype PairLfo = PairLfo { unPairLfo :: Pair AuxLfo Lfo }
  deriving stock (Show)
  deriving newtype (Eq, Default)

instance Binary PairLfo where
  get = do
    -- 212
    lfoType <- get @LfoType
    -- 213
    lfoSteps <- get @LfoSteps
    -- 215-215
    auxLfo214To215 <- get
    -- 216-129
    lfoAmount <- get
    let !auxLfo = AuxLfo {..}
        !lfo = Lfo {..}
        !pair = Pair auxLfo lfo
    pure $! PairLfo pair
  put (PairLfo (Pair (AuxLfo {..}) (Lfo {..}))) = do
    -- 212
    put lfoType
    -- 213
    put lfoSteps
    -- 215-215
    put auxLfo214To215
    -- 216-129
    put lfoAmount

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
  } deriving stock (Eq, Show)

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

numSlices :: Int
numSlices = 48

data Slices = Slices
  { slicesAdjust :: !(VP.Vector Word16LE)
  -- ^ Must be 48 elements long
  , slicesNumber :: !Word8
  , slicesActive :: !Word8
  } deriving stock (Eq, Show)

instance Default Slices where
  def = Slices (VP.replicate numSlices 0) 0 0

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
  } deriving stock (Eq, Show)

instance Default Granular where
  def = Granular 441 0 def def

data Effects = Effects
  { effReverbSend :: !Word8
  , effOverdrive :: !Word8
  , effBitDepth :: !Word8
  } deriving stock (Eq, Show)

instance Default Effects where
  def = Effects 0 0 16

data Header = Header
  { headPreamble :: !Preamble
  , headVolumeAuto :: !Auto
  , headPanningAuto :: !Auto
  , headCutoffAuto :: !Auto
  , headWavetablePositionAuto :: !Auto
  , headGranularPositionAuto :: !Auto
  , headFinetuneAuto :: !Auto
  , headVolumeLfo :: !Lfo
  , headPanningLfo :: !Lfo
  , headCutoffLfo :: !Lfo
  , headWavetablePositionLfo :: !Lfo
  , headGranularPositionLfo :: !Lfo
  , headFinetuneLfo :: !Lfo
  , headFilter :: !Filter
  , headInstParams :: !InstParams
  , headSlices :: !Slices
  } deriving stock (Eq, Show)

instance Default Header where
  def = Header def def def def def def def def def def def def def def def def

data AuxHeader = AuxHeader
  {
  } deriving stock (Eq, Show)

data Pti = Pti
  { ptiHeader :: !Header
  , ptiWav :: !(Wav Int16LE)
  } deriving stock (Eq, Show)

instance Default Pti where
  def = Pti def def

getPairHeader :: Get (Pair AuxHeader Header)
getPairHeader = undefined

decodePairPti :: Monad m => DecodeT m (Pair AuxHeader Pti)
decodePairPti = do
  Pair auxHd hd <- decodeGet getPairHeader
  wav <- decodeSpecificWav Proxy
  pure $! Pair auxHd (Pti hd wav)
