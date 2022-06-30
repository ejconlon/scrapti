module Scrapti.Tracker.Mtp where

import Dahdit (Binary, BinaryRep (..), ByteSized, StaticByteSized, StaticBytes, StaticSeq, ViaBinaryRep (..),
               ViaStaticGeneric (..), Word8)
import Data.Default (Default (..))
import GHC.Generics (Generic)
import Scrapti.Binary (ExactBytes)
import Scrapti.Tracker.Checked (Checked(..), mkChecked)

data FxType =
    FxTypeVolume
  | FxTypePanning
  | FxTypeChord
  | FxTypeArp
  deriving stock (Eq, Show)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaBinaryRep FxType)

instance BinaryRep Word8 FxType where
  fromBinaryRep = error "TODO"
  toBinaryRep = error "TODO"
  -- vol  = 0x12
  -- panning = 0x1F
  -- chord = 0x0E  # dec 14
  -- arp = 0x15

newtype Note = Node { unNode :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, StaticByteSized, Binary)

newtype Inst = Inst { unInst :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, StaticByteSized, Binary)

data Fx = Fx
  { fxType :: !FxType
  , fxValue :: !Word8
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Fx)

data Step = Step
  { stepNote :: !Note
  , stepInst :: !Inst
  , stepFx1 :: !Fx
  , stepFx2 :: !Fx
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
  { mtpbFileType :: !(ExactBytes "KS")
  , mtpbAux2To28 :: !(StaticBytes 26)
  , mtpbPattern :: !(StaticSeq 8 Track)
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric MtpBody)

-- TODO
instance Default MtpBody where
  def = MtpBody def def def

newtype Mtp = Mtp { unMtp :: Checked MtpBody }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, StaticByteSized, Binary)

instance Default Mtp where
  def = Mtp (mkChecked def)
