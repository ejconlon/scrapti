{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Pti where

import Data.Binary (Binary (..))
import Data.Default (Default (..))
import Data.Int (Int16, Int8)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word16, Word32, Word8)
import Scrapti.Binary (DecodeT, Get, decodeGet)
import Scrapti.Classes (BinaryRep (..), ViaBinaryRep (..), ViaBoundedEnum (..))
import Scrapti.Wav (Wav, decodeSpecificWav)

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

instance BinaryRep Word16 WavetableWindowSize where
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
  { preIsWavetable :: !Bool
  , preName :: !Text
  , preSampleLength :: !Word32
  , preWavetableWindowSize :: !WavetableWindowSize
  , preWavetableTotalPositions :: !Word16
  , preSamplePlayback :: !SamplePlayback
  , prePlaybackStart :: !Word16
  , preLoopStart :: !Word16
  , preLoopEnd :: !Word16
  , prePlaybackEnd :: !Word16
  , preWavetablePosition :: !Word16
  } deriving stock (Eq, Show)

instance Default Preamble where
  def = Preamble False "" 0 def 0 def 0 1 65534 65535 0

data AutoEnvelope = AutoEnvelope
  { aeAmount :: Float
  , aeAttack :: !Word16
  , aeDecay :: !Word16
  , aeSustain :: !Float
  , aeRelease :: !Word16
  } deriving stock (Eq, Show)

instance Default AutoEnvelope where
  def = AutoEnvelope 1.0 0 0 1.0 1000

data AutoType =
    ATOff
  | ATEnvelope
  | ATLfo
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving Binary via (ViaBinaryRep AutoType)

instance Default AutoType where
  def = ATEnvelope

instance BinaryRep Word16 AutoType where
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
  , lfoAmount :: !Float
  } deriving stock (Eq, Show)

instance Default Lfo where
  def = Lfo def def 0.5

data FilterType =
    FTDisabled
  | FTLowPass
  | FTHighPass
  | FTBandPass
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving Binary via (ViaBinaryRep FilterType)

instance Default FilterType where
  def = FTDisabled

instance BinaryRep Word16 FilterType where
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
  { slicesAdjust :: !(VU.Vector Word16)
  -- ^ Must be 48 elements long
  , slicesNumber :: !Word8
  , slicesActive :: !Word8
  } deriving stock (Eq, Show)

instance Default Slices where
  def = Slices (VU.replicate numSlices 0) 0 0

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
  { granLen :: !Word16
  , granPos :: !Word16
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

data Aux = Aux
  { auxMajVer :: !Word8
  , auxMinVer :: !Word8
  } deriving stock (Eq, Show)

data WithAux a = WithAux !Aux !a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Pti = Pti
  { ptiHeader :: !Header
  , ptiWav :: !(Wav Int16)
  } deriving stock (Eq, Show)

instance Default Pti where
  def = Pti def def

decodeAuxHeader :: Monad m => DecodeT m (WithAux Header)
decodeAuxHeader = decodeGet getAuxHeader

getAuxHeader :: Get (WithAux Header)
getAuxHeader = do
  undefined
  -- getExpect "0-1: header" (getByteString 2) "PT"
  -- getExpect "2: ?" getWord8 1
  -- getExpect "3: ?" getWord8 0
  -- majVer <- getWord8
  -- minVer <- getWord8
  -- -- getExpect "6: ?" getWord8 0
  -- -- getExpect "7: ?" getWord8 1
  -- pure $! PtiHeader
  --   majVer
  --   minVer

decodeAuxPti :: Monad m => DecodeT m (WithAux Pti)
decodeAuxPti = do
  WithAux aux hd <- decodeAuxHeader
  wav <- decodeSpecificWav Proxy
  pure $! WithAux aux (Pti hd wav)
