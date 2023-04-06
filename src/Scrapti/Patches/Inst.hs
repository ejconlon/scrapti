{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Patches.Inst where

import AesonVia (AesonRecord (..), AesonTag (..), HasTagPrefix (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict')
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Default (Default (..))
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import GHC.Generics (Generic)
import Scrapti.Midi.Notes (LinNote (..))

-- Tempo in BPM
newtype Tempo = Tempo {unTempo :: Rational}
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

instance Default Tempo where
  def = Tempo 120

-- tempo = 120 bpm how long does a beat take? in seconds per beat 1 minute (60 s) a 120 beats/min
-- is 60 s/min / 120 beats/min
beatPeriod :: Tempo -> Rational
beatPeriod (Tempo t) = 60 / t

data InstEnv = InstEnv
  { ieAttack :: !Rational
  , ieDecay :: !Rational
  , ieSustain :: !Rational
  , ieRelease :: !Rational
  , ieDepth :: !Rational
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstEnv)

data InstLfoWave
  = InstLfoRevSaw
  | InstLfoSaw
  | InstLfoTriangle
  | InstLfoSquare
  | InstLfoRandom
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonTag InstLfoWave)

instance HasTagPrefix InstLfoWave where
  getTagPrefix _ = "InstLfo"

data InstLfo = InstLfo
  { ilWave :: InstLfoWave
  , ilFreq :: !Rational
  , ilDepth :: !Rational
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstLfo)

data InstAutoType
  = InstAutoTypeOff
  | InstAutoTypeEnv
  | InstAutoTypeLfo
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonTag InstAutoType)

instance HasTagPrefix InstAutoType where
  getTagPrefix _ = "InstAutoType"

instance Default InstAutoType where
  def = InstAutoTypeOff

data InstAuto = InstAuto
  { iaType :: !InstAutoType
  , iaEnv :: !(Maybe InstEnv)
  , iaLfo :: !(Maybe InstLfo)
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstAuto)

instance Default InstAuto where
  def = InstAuto def Nothing Nothing

data InstAutoTarget
  = InstAutoTargetVolume
  | InstAutoTargetPanning
  | InstAutoTargetCutoff
  | InstAutoTargetFinetune
  deriving stock (Eq, Enum, Bounded, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonTag InstAutoTarget)

instance HasTagPrefix InstAutoTarget where
  getTagPrefix _ = "InstAutoTarget"

data InstBlock a = InstBlock
  { ibVolume :: !a
  , ibPanning :: !a
  , ibCutoff :: !a
  , ibFinetune :: !a
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord (InstBlock a))

pureInstBlock :: a -> InstBlock a
pureInstBlock a = InstBlock a a a a

instance Default a => Default (InstBlock a) where
  def = pureInstBlock def

data InstFilterType
  = InstFilterTypeLowpass
  | InstFilterTypeHighpass
  | InstFilterTypeBandpass
  deriving stock (Eq, Enum, Bounded, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonTag InstFilterType)

instance HasTagPrefix InstFilterType where
  getTagPrefix _ = "InstFilter"

data InstFilter = InstFilter
  { ifType :: !InstFilterType
  , ifCutoff :: !Rational
  , ifResonance :: !Rational
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstFilter)

data InstConfig = InstConfig
  { icPanning :: !Rational
  , icTune :: !Rational
  , icFilter :: !(Maybe InstFilter)
  , icAuto :: !(InstBlock InstAuto)
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstConfig)

instance Default InstConfig where
  def = InstConfig 0 0 Nothing def

data InstKeyRange = InstKeyRange
  { ikrLowkey :: !Integer
  , ikrSampKey :: !Integer
  , ikrHighkey :: !Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstKeyRange)

data InstLoopType
  = InstLoopTypeForward
  | InstLoopTypeBackward
  | InstLoopTypeAlternate
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonTag InstLoopType)

instance HasTagPrefix InstLoopType where
  getTagPrefix _ = "InstLoopType"

data InstLoop = InstLoop
  { ilType :: !InstLoopType
  , ilLoopStart :: !Integer
  , ilLoopEnd :: !Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstLoop)

data InstCrop = InstCrop
  { icStart :: !Integer
  , icEnd :: !Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstCrop)

data InstRegion x = InstRegion
  { irSample :: !x
  , irKeyRange :: !InstKeyRange
  , irLoop :: !(Maybe InstLoop)
  , irCrop :: !(Maybe InstCrop)
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord (InstRegion x))

data InstSpec x = InstSpec
  { isConfig :: !InstConfig
  , isRegions :: !(Seq (InstRegion x))
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord (InstSpec x))

annotateNotes :: InstSpec x -> InstSpec (LinNote, x)
annotateNotes (InstSpec config regions) = InstSpec config (fmap go regions)
 where
  go reg = let note = LinNote (fromInteger (ikrSampKey (irKeyRange reg))) in reg {irSample = (note, irSample reg)}

data InstControl = InstControl
  { icTempo :: !(Maybe Tempo)
  , icPath :: !(Maybe FilePath)
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstControl)

instance Default InstControl where
  def = InstControl Nothing Nothing

data InstDef x = InstDef
  { idControl :: !InstControl
  , idSpec :: !(InstSpec x)
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord (InstDef x))

instToJson :: ToJSON x => InstDef x -> Text
instToJson = TL.toStrict . TLB.toLazyText . encodePrettyToTextBuilder

jsonToInst :: FromJSON x => Text -> Either String (InstDef x)
jsonToInst = eitherDecodeStrict' . TE.encodeUtf8

traverseBlock :: Applicative m => (InstAutoTarget -> InstAuto -> m InstAuto) -> InstBlock InstAuto -> m (InstBlock InstAuto)
traverseBlock onAuto (InstBlock volAuto panAuto cutoffAuto tuneAuto) =
  InstBlock
    <$> onAuto InstAutoTargetVolume volAuto
    <*> onAuto InstAutoTargetPanning panAuto
    <*> onAuto InstAutoTargetCutoff cutoffAuto
    <*> onAuto InstAutoTargetFinetune tuneAuto

traverseBlock_ :: Applicative m => (InstAutoTarget -> InstAuto -> m ()) -> InstBlock InstAuto -> m ()
traverseBlock_ onAuto (InstBlock volAuto panAuto cutoffAuto tuneAuto) =
  onAuto InstAutoTargetVolume volAuto
    *> onAuto InstAutoTargetPanning panAuto
    *> onAuto InstAutoTargetCutoff cutoffAuto
    *> onAuto InstAutoTargetFinetune tuneAuto
