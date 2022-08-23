module Scrapti.Patches.Inst where

import Data.Default (Default (..))
import Data.Sequence (Seq)
import Data.Text (Text)

data InstEnv = InstEnv
  { ieAttack :: !Rational
  , ieDecay :: !Rational
  , ieSustain :: !Rational
  , ieRelease :: !Rational
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

-- | sfz mapping: volume, pan, cutoff, pitch
data InstBlock a = InstBlock
  { ibVolume :: !a
  , ibPanning :: !a
  , ibCutoff :: !a
  , ibFinetune :: !a
  } deriving stock (Eq, Show)

pureInstBlock :: a -> InstBlock a
pureInstBlock a = InstBlock a a a a

data InstFilterType =
    InstFilterTypeLowpass
  | InstFilterTypeHighpass
  | InstFilterTypeBandpass
  deriving stock (Eq, Enum, Bounded, Show)

data InstFilter = InstFilter
  { ifType :: !InstFilterType
  , ifAttack :: !Rational
  , ifDecay :: !Rational
  , ifSustain :: !Rational
  , ifRelease :: !Rational
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
  , ikrMidKey :: !Integer
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
