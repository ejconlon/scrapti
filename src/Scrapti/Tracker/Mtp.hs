module Scrapti.Tracker.Mtp where

import Dahdit (Binary, BinaryRep (..), ByteSized, StaticByteSized, StaticBytes, StaticSeq, ViaBinaryRep (..),
               ViaStaticGeneric (..), Word8)
import Data.Default (Default (..))
import GHC.Generics (Generic)
import Scrapti.Binary (ExactBytes)
import Scrapti.Tracker.Checked (Checked(..))

data EffectType =
    EffectTypeVolume
  | EffectTypePanning
  | EffectTypeChord
  | EffectTypeArp
  deriving stock (Eq, Show)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaBinaryRep EffectType)

instance BinaryRep Word8 EffectType where
  fromBinaryRep = error "TODO"
  toBinaryRep = error "TODO"
  -- vol  = 0x12
  -- panning = 0x1F
  -- chord = 0x0E  # dec 14
  -- arp = 0x15

-- data StepEffectType =

newtype Note = Node { unNode :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, StaticByteSized, Binary)

newtype Instrument = Instrument { unInstrument :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, StaticByteSized, Binary)

-- TODO
data StepEffect = StepEffect
  { stepEffectType :: !Word8
  , stepEffectValue :: !Word8
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric StepEffect)

data Step = Step
  { stepNote :: !Note
  , stepInstrument :: !Instrument
  , stepEffect1 :: !StepEffect
  , stepEffect2 :: !StepEffect
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Step)

-- TODO
data Track = Track
  { trackX :: !Word8
  , trackY :: !Word8
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Track)

instance Default Track where
  def = Track 0 0

data MtpBody = MtpBody
  { mbFileType :: !(ExactBytes "KS")
  , mbAuxHeader :: !(StaticBytes 26)
  , mbPattern :: !(StaticSeq 8 Track)
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric MtpBody)

newtype Mtp = Mtp { unMtp :: Checked MtpBody }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, StaticByteSized, Binary)
