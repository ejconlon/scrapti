{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Scrapti.Tracker.Pti
  ( WavetableWindowSize (..)
  , SamplePlayback (..)
  , Preamble (..)
  , AutoEnvelope (..)
  , AutoType (..)
  , Auto (..)
  , LfoType (..)
  , LfoSteps (..)
  , Lfo (..)
  , FilterType (..)
  , Filter (..)
  , InstParams (..)
  , Slices (..)
  , GranularShape (..)
  , GranularLoopMode (..)
  , Granular (..)
  , Effects (..)
  , Block (..)
  , mkBlock
  , defAutoBlock
  , Header (..)
  , Pti (..)
  , mkPti
  )
where

import Dahdit
  ( Binary (..)
  , BinaryRep (..)
  , BoolByte (..)
  , ExactBytes
  , FloatLE
  , Int16LE
  , LiftedPrimArray
  , ShortByteString
  , StaticArray
  , StaticByteSized (..)
  , StaticBytes (..)
  , ViaBinaryRep (..)
  , ViaBoundedEnum (..)
  , ViaStaticGeneric (..)
  , Word16LE
  , Word32LE
  , getLiftedPrimArray
  , getRemainingLiftedPrimArray
  , putLiftedPrimArray
  , sizeofLiftedPrimArray
  )
import Data.Default (Default (..))
import Data.Int (Int8)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Scrapti.Binary (QuietLiftedArray (..))
import Scrapti.Tracker.Checked (Checked (..), mkChecked, updateCheckedCode, verifyCheckedCode)

data WavetableWindowSize
  = WWS32
  | WWS64
  | WWS128
  | WWS256
  | WWS1024
  | WWS2048
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word16LE WavetableWindowSize)

instance BinaryRep Word16LE WavetableWindowSize where
  fromBinaryRep = \case
    32 -> Right WWS32
    64 -> Right WWS64
    128 -> Right WWS128
    256 -> Right WWS256
    1024 -> Right WWS1024
    2048 -> Right WWS2048
    other -> Left ("invalid wavetable window size: " ++ show other)
  toBinaryRep = \case
    WWS32 -> 32
    WWS64 -> 64
    WWS128 -> 128
    WWS256 -> 256
    WWS1024 -> 1024
    WWS2048 -> 2048

data SamplePlayback
  = SPOneShot
  | SPForwardLoop
  | SPBackwardLoop
  | SPPingPongLoop
  | SPSlice
  | SPBeatSlice
  | SPWavetable
  | SPGranular
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 SamplePlayback)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 SamplePlayback)

data Preamble = Preamble
  { preFileType :: !(ExactBytes 2 "TI")
  , -- 0-1
    preAux2To19 :: !(StaticBytes 18)
  , -- 2-19
    preIsWavetable :: !BoolByte
  , -- 20
    preName :: !(StaticBytes 31)
  , -- 21-51
    preAux52To59 :: !(StaticBytes 8)
  , -- 52-59
    preSampleLength :: !Word32LE
  , -- 60-63
    preWavetableWindowSize :: !WavetableWindowSize
  , -- 64-65
    preAux66To67 :: !Word16LE
  , -- 66-67
    preWavetableTotalPositions :: !Word16LE
  , -- 68-69
    preAux70To75 :: !(StaticBytes 6)
  , -- 70-75
    preSamplePlayback :: !SamplePlayback
  , -- 76
    preAux77 :: !Word8
  , -- 77
    prePlaybackStart :: !Word16LE
  , -- 78-79
    preLoopStart :: !Word16LE
  , -- 80-81
    preLoopEnd :: !Word16LE
  , -- 82-83
    prePlaybackEnd :: !Word16LE
  , -- 84-85
    preAux86To87 :: !Word16LE
  , -- 86-87
    preWavetablePosition :: !Word16LE
  , -- 88-89
    preAux90To91 :: !Word16LE
    -- 90-91
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Preamble)

fw15Preamble, fw16Preamble :: ShortByteString
fw15Preamble = "\SOH\NUL\SOH\ENQ\NUL\SOH\t\t\t\tt\SOHff\SOH\NUL\NUL\NUL"
fw16Preamble = "\SOH\NUL\SOH\ACK\NUL\SOH\t\t\t\tt\SOH\255\255\SOH\NUL\NUL\NUL"

instance Default Preamble where
  def =
    Preamble
      { preFileType = def
      , preAux2To19 = StaticBytes fw16Preamble
      , preIsWavetable = BoolByte False
      , preName = ""
      , preAux52To59 = "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
      , preSampleLength = 0
      , preWavetableWindowSize = WWS2048
      , preAux66To67 = 0
      , preWavetableTotalPositions = 0
      , preAux70To75 = "\NUL\NUL\NUL\NUL\NUL\NUL"
      , preSamplePlayback = SPOneShot
      , preAux77 = 0
      , prePlaybackStart = 0
      , preLoopStart = 1
      , preLoopEnd = 65534
      , prePlaybackEnd = 65535
      , preAux86To87 = 0
      , preWavetablePosition = 0
      , preAux90To91 = 0
      }

data AutoEnvelope = AutoEnvelope
  { aeAmount :: !FloatLE
  , -- 92-95
    aeAux96To97 :: !Word16LE
  , -- 96-97
    aeAttack :: !Word16LE
  , -- 98-99
    aeAux100To101 :: !Word16LE
  , -- 100-101
    aeDecay :: !Word16LE
  , -- 102-103
    aeSustain :: !FloatLE
  , -- 104-107
    aeRelease :: !Word16LE
    -- 108-109
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric AutoEnvelope)

instance Default AutoEnvelope where
  def =
    AutoEnvelope
      { aeAmount = 1.0
      , aeAux96To97 = 0
      , aeAttack = 0
      , aeAux100To101 = 0
      , aeDecay = 0
      , aeSustain = 1.0
      , aeRelease = 1000
      }

data AutoType
  = ATOff
  | ATEnvelope
  | ATLfo
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word16LE AutoType)

instance Default AutoType where
  def = ATOff

instance BinaryRep Word16LE AutoType where
  fromBinaryRep = \case
    0x0000 -> Right ATOff
    0x0100 -> Right ATEnvelope
    0x0101 -> Right ATLfo
    other -> Left ("Invalid automation type: " ++ show other)
  toBinaryRep = \case
    ATOff -> 0x0000
    ATEnvelope -> 0x0100
    ATLfo -> 0x0101

data Auto = Auto
  { autoEnvelope :: !AutoEnvelope
  , autoType :: !AutoType
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Auto)

instance Default Auto where
  def = Auto def def

data LfoType
  = LTRevSaw
  | LTSaw
  | LTTriangle
  | LTSquare
  | LTRandom
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 LfoType)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 LfoType)

data LfoSteps
  = LS24
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
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 LfoSteps)

data Lfo = Lfo
  { lfoType :: !LfoType
  , -- 212
    lfoSteps :: !LfoSteps
  , -- 213
    lfoAux214To215 :: Word16LE
  , -- 214-215
    lfoAmount :: !FloatLE
    -- 216-219
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Lfo)

instance Default Lfo where
  def =
    Lfo
      { lfoType = LTTriangle
      , lfoSteps = LS24
      , lfoAux214To215 = 0
      , lfoAmount = 0.5
      }

data FilterType
  = FTDisabled
  | FTLowPass
  | FTHighPass
  | FTBandPass
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word16LE FilterType)

instance Default FilterType where
  def = FTDisabled

instance BinaryRep Word16LE FilterType where
  fromBinaryRep = \case
    0x0000 -> pure FTDisabled
    0x0100 -> pure FTLowPass
    0x0101 -> pure FTHighPass
    0x0102 -> pure FTBandPass
    other -> Left ("invalid filter type: " ++ show other)
  toBinaryRep = \case
    FTDisabled -> 0x0000
    FTLowPass -> 0x0100
    FTHighPass -> 0x0101
    FTBandPass -> 0x0102

data Filter = Filter
  { filtCutoff :: !FloatLE
  , filtResonance :: !FloatLE
  , filtType :: !FilterType
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Filter)

instance Default Filter where
  def =
    Filter
      { filtCutoff = 1.0
      , filtResonance = 0.0
      , filtType = def
      }

data InstParams = InstParams
  { ipTune :: !Int8
  , -- 270
    ipFineTune :: !Int8
  , -- 271
    ipVolume :: !Word8
  , -- 272
    ipAux273To275 :: !(StaticBytes 3)
  , -- 273-275
    ipPanning :: !Word8
  , -- 276
    ipAux277 :: !Word8
  , -- 277
    ipDelaySend :: !Word8
  , -- 278
    ipAux279 :: !Word8
    -- 279
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric InstParams)

instance Default InstParams where
  def =
    InstParams
      { ipTune = 0
      , ipFineTune = 0
      , ipVolume = 50
      , ipAux273To275 = "\NUL\NUL\NUL"
      , ipPanning = 50
      , ipAux277 = 0
      , ipDelaySend = 0
      , ipAux279 = 0
      }

data Slices = Slices
  { slicesAdjust :: !(StaticArray 48 Word16LE)
  , slicesNumber :: !Word8
  , slicesActive :: !Word8
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Slices)

instance Default Slices where
  def =
    Slices
      { slicesAdjust = def
      , slicesNumber = 0
      , slicesActive = 0
      }

data GranularShape
  = GSSquare
  | GSTriangle
  | GSGauss
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 GranularShape)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 GranularShape)

instance Default GranularShape where
  def = GSSquare

data GranularLoopMode
  = GLMForward
  | GLMBackward
  | GLMPingPong
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 GranularLoopMode)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 GranularLoopMode)

instance Default GranularLoopMode where
  def = GLMForward

data Granular = Granular
  { granLen :: !Word16LE
  , granPos :: !Word16LE
  , granShape :: !GranularShape
  , granLoopMode :: !GranularLoopMode
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Granular)

instance Default Granular where
  def =
    Granular
      { granLen = 441
      , granPos = 0
      , granShape = def
      , granLoopMode = def
      }

data Effects = Effects
  { effReverbSend :: !Word8
  , -- 384
    effOverdrive :: !Word8
  , -- 385
    effBitDepth :: !Word8
  , -- 386
    effAux387 :: !Word8
    -- 387
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Effects)

instance Default Effects where
  def =
    Effects
      { effReverbSend = 0
      , effOverdrive = 0
      , effBitDepth = 16
      , effAux387 = 0
      }

data Block a = Block
  { blockVolume :: !a
  , blockPanning :: !a
  , blockCutoff :: !a
  , blockWavetablePosition :: !a
  , blockGranularPosition :: !a
  , blockFinetune :: !a
  }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric (Block a))

mkBlock :: a -> Block a
mkBlock a = Block a a a a a a

instance (Default a) => Default (Block a) where
  def = mkBlock def

defAutoBlock :: Block Auto
defAutoBlock =
  let envVol = def @AutoEnvelope
      autoVol = Auto envVol ATEnvelope
      envRest = envVol {aeAttack = 3000}
      autoRest = Auto envRest ATOff
      block = mkBlock autoRest
  in  block {blockVolume = autoVol}

data Header = Header
  { hdrPreamble :: !Preamble
  , hdrAutoBlock :: !(Block Auto)
  , hdrLfoBlock :: !(Block Lfo)
  , hdrFilter :: !Filter
  , hdrInstParams :: !InstParams
  , hdrSlices :: !Slices
  , hdrGranular :: !Granular
  , hdrEffects :: !Effects
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Header)

instance Default Header where
  def = Header def defAutoBlock def def def def def def

data Pti = Pti
  { ptiHeader :: !(Checked Header)
  , ptiPcmData :: !(QuietLiftedArray Int16LE)
  }
  deriving stock (Eq, Show)

updatePtiCode :: Pti -> Pti
updatePtiCode pti = pti {ptiHeader = updateCheckedCode (ptiHeader pti)}

verifyPtiCode :: Pti -> Bool
verifyPtiCode pti = verifyCheckedCode (ptiHeader pti)

instance Binary Pti where
  byteSize (Pti header (QuietLiftedArray pcmData)) = byteSize header + sizeofLiftedPrimArray pcmData
  get = do
    header <- get
    let !sampleLength = fromIntegral (preSampleLength (hdrPreamble (checkedVal header)))
    pcmData <-
      if sampleLength == 0 -- what the heck, it happens
        then getRemainingLiftedPrimArray (Proxy :: Proxy Int16LE)
        else getLiftedPrimArray (Proxy :: Proxy Int16LE) sampleLength
    pure $! Pti header (QuietLiftedArray pcmData)
  put (Pti header (QuietLiftedArray pcmData)) = do
    put header
    putLiftedPrimArray pcmData

instance Default Pti where
  def = Pti def def

mkPti :: Header -> LiftedPrimArray Int16LE -> Pti
mkPti hd = Pti (mkChecked hd) . QuietLiftedArray
