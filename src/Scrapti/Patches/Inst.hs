{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Patches.Inst where

import AesonVia (AesonRecord (..), AesonTag (..), HasTagPrefix (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict')
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Default (Default (..))
import Data.Foldable (traverse_)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import GHC.Generics (Generic)
import Scrapti.Midi.Notes (LinNote (..))

data InstEnv = InstEnv
  { ieAttack :: !Rational
  , ieDecay :: !Rational
  , ieSustain :: !Rational
  , ieRelease :: !Rational
  , ieDepth :: !Rational
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord InstEnv)

data InstLfoWave =
    InstLfoRevSaw
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
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord InstLfo)

data InstAuto =
    InstAutoEnv !InstEnv
  | InstAutoLfo !InstLfo
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (AesonRecord InstAuto)

data InstAutoTarget =
    InstAutoTargetVolume
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
  } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord (InstBlock a))

pureInstBlock :: a -> InstBlock a
pureInstBlock a = InstBlock a a a a

data InstFilterType =
    InstFilterTypeLowpass
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
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord InstFilter)

data InstConfig = InstConfig
  { icPanning :: !Rational
  , icTune :: !Rational
  , icFilter :: !(Maybe InstFilter)
  , icAuto :: !(InstBlock (Maybe InstAuto))
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord InstConfig)

instance Default InstConfig where
  def = InstConfig 0 0 Nothing (pureInstBlock Nothing)

data InstKeyRange = InstKeyRange
  { ikrLowkey :: !Integer
  , ikrSampKey :: !Integer
  , ikrHighkey :: !Integer
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord InstKeyRange)

data InstLoopType =
    InstLoopTypeForward
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
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord InstLoop)

data InstCrop = InstCrop
  { icStart :: !Integer
  , icEnd :: !Integer
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord InstCrop)

data InstRegion x = InstRegion
  { irSample :: !x
  , irKeyRange :: !InstKeyRange
  , irLoop :: !(Maybe InstLoop)
  , irCrop :: !(Maybe InstCrop)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord (InstRegion x))

data InstSpec x = InstSpec
  { isConfig :: !InstConfig
  , isRegions :: !(Seq (InstRegion x))
  } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord (InstSpec x))

annotateNotes :: InstSpec x -> InstSpec (LinNote, x)
annotateNotes (InstSpec config regions) = InstSpec config (fmap go regions) where
  go reg = let note = LinNote (fromInteger (ikrSampKey (irKeyRange reg))) in reg { irSample = (note, irSample reg) }

data InstDef x = InstDef
  { idPath :: !(Maybe FilePath)
  , idSpec :: !(InstSpec x)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord (InstDef x))

instToJson :: ToJSON x => InstDef x -> Text
instToJson = TL.toStrict . TLB.toLazyText . encodePrettyToTextBuilder

jsonToInst :: FromJSON x => Text -> Either String (InstDef x)
jsonToInst = eitherDecodeStrict' . TE.encodeUtf8

traverseBlock :: Applicative m => (InstAutoTarget -> InstAuto -> m InstAuto) -> InstBlock (Maybe InstAuto) -> m (InstBlock (Maybe InstAuto))
traverseBlock onAuto (InstBlock volAuto panAuto cutoffAuto tuneAuto) =
  InstBlock
  <$> traverse (onAuto InstAutoTargetVolume) volAuto
  <*> traverse (onAuto InstAutoTargetPanning) panAuto
  <*> traverse (onAuto InstAutoTargetCutoff) cutoffAuto
  <*> traverse (onAuto InstAutoTargetFinetune) tuneAuto

traverseBlock_ :: Applicative m => (InstAutoTarget -> InstAuto -> m ()) -> InstBlock (Maybe InstAuto) -> m ()
traverseBlock_ onAuto (InstBlock volAuto panAuto cutoffAuto tuneAuto) =
  traverse_ (onAuto InstAutoTargetVolume) volAuto *>
  traverse_ (onAuto InstAutoTargetPanning) panAuto *>
  traverse_ (onAuto InstAutoTargetCutoff) cutoffAuto *>
  traverse_ (onAuto InstAutoTargetFinetune) tuneAuto
