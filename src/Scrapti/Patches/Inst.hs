module Scrapti.Patches.Inst where

import Data.Default (Default (..))
import Data.Foldable (traverse_)
import Data.Sequence (Seq)

data InstEnv = InstEnv
  { ieAttack :: !Rational
  , ieDecay :: !Rational
  , ieSustain :: !Rational
  , ieRelease :: !Rational
  , ieDepth :: !Rational
  } deriving stock (Eq, Show)

data InstLfoWave =
    InstLfoRevSaw
  | InstLfoSaw
  | InstLfoTriangle
  | InstLfoSquare
  | InstLfoRandom
  deriving stock (Eq, Show)

data InstLfo = InstLfo
  { ilWave :: InstLfoWave
  , ilFreq :: !Rational
  , ilDepth :: !Rational
  } deriving stock (Eq, Show)

data InstAuto =
    InstAutoEnv !InstEnv
  | InstAutoLfo !InstLfo
  deriving stock (Eq, Show)

data InstAutoTarget =
    InstAutoTargetVolume
  | InstAutoTargetPanning
  | InstAutoTargetCutoff
  | InstAutoTargetFinetune
  deriving stock (Eq, Enum, Bounded, Show)

data InstBlock a = InstBlock
  { ibVolume :: !a
  , ibPanning :: !a
  , ibCutoff :: !a
  , ibFinetune :: !a
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

pureInstBlock :: a -> InstBlock a
pureInstBlock a = InstBlock a a a a

data InstFilterType =
    InstFilterTypeLowpass
  | InstFilterTypeHighpass
  | InstFilterTypeBandpass
  deriving stock (Eq, Enum, Bounded, Show)

data InstFilter = InstFilter
  { ifType :: !InstFilterType
  , ifCutoff :: !Rational
  , ifResonance :: !Rational
  } deriving stock (Eq, Show)

data InstParams = InstParams
  { ipPanning :: !Rational
  , ipTune :: !Rational
  , ipFilter :: !(Maybe InstFilter)
  , ipAuto :: !(InstBlock (Maybe InstAuto))
  } deriving stock (Eq, Show)

instance Default InstParams where
  def = InstParams 0 0 Nothing (pureInstBlock Nothing)

data InstKeyRange = InstKeyRange
  { ikrLowkey :: !Integer
  , ikrSampKey :: !Integer
  , ikrHighkey :: !Integer
  } deriving stock (Eq, Show)

data InstLoopType =
    InstLoopTypeForward
  | InstLoopTypeBackward
  | InstLoopTypeAlternate
  deriving stock (Eq, Show)

data InstLoop = InstLoop
  { ilType :: !InstLoopType
  , ilLoopStart :: !Integer
  , ilLoopEnd :: !Integer
  } deriving stock (Eq, Show)

data InstCrop = InstCrop
  { icStart :: !Integer
  , icEnd :: !Integer
  } deriving stock (Eq, Show)

data InstRegion x = InstRegion
  { irSample :: !x
  , irKeyRange :: !InstKeyRange
  , irLoop :: !(Maybe InstLoop)
  , irCrop :: !(Maybe InstCrop)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

data InstSpec x = InstSpec
  { isParams :: !InstParams
  , isRegions :: !(Seq (InstRegion x))
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

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

-- data InstSpecAlg x y m = InstSpecAlg
--   { isaOnParams :: !(InstParams -> m InstParams)
--   , isaOnRegion :: !(Int -> Int -> InstRegion x -> m (InstRegion y))
--   }

-- runInstSpecAlg :: Applicative m => InstSpecAlg x y m -> InstSpec x -> m (InstSpec y)
-- runInstSpecAlg (InstSpecAlg onParams onRegion) (InstSpec params regions) =
--   let !numRegions = Seq.length regions
--   in InstSpec
--     <$> onParams params
--     <*> Seq.traverseWithIndex (onRegion numRegions) regions

-- data InstRegionAlg x y m = InstRegionAlg
--   { iraOnSample :: !(x -> m y)
--   , iraOnKeyRange :: !(InstKeyRange -> m InstKeyRange)
--   , iraOnLoop :: !(InstLoop -> m InstLoop)
--   , iraOnCrop :: !(InstCrop -> m InstCrop)
--   }

-- runInstRegionAlg :: Applicative m => InstRegionAlg x y m -> InstRegion x -> m (InstRegion y)
-- runInstRegionAlg (InstRegionAlg onSample onKeyRange onLoop onCrop) (InstRegion sample keyRange mayLoop mayCrop) =
--   InstRegion
--   <$> onSample sample
--   <*> onKeyRange keyRange
--   <*> traverse onLoop mayLoop
--   <*> traverse onCrop mayCrop

-- data InstParamsAlg m = InstParamsAlg
--   { ipaOnPanning :: !(Rational -> m Rational)
--   , ipaOnTune :: !(Rational -> m Rational)
--   , ipaOnFilter :: !(InstFilter -> m InstFilter)
--   , ipaOnAuto :: !(InstAutoTarget -> InstAuto -> m InstAuto)
--   }

-- runInstParamsAlg :: Applicative m => InstParamsAlg m -> InstParams -> m InstParams
-- runInstParamsAlg (InstParamsAlg onPanning onTune onFilt onAuto) (InstParams panning tune filt block) =
--   InstParams
--   <$> onPanning panning
--   <*> onTune tune
--   <*> traverse onFilt filt
--   <*> traverseBlock onAuto block
