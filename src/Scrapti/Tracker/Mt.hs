module Scrapti.Tracker.Mt
  ( MtBody (..)
  , Mt (..)
  ) where

import Dahdit (Binary, ByteSized, StaticByteSized, ViaStaticGeneric (..), StaticBytes)
import GHC.Generics (Generic)
import Scrapti.Binary (ExactBytes)
import Scrapti.Tracker.Checked (Checked(..))

data MtBody = MtBody
  { mtbFileType :: !(ExactBytes "MT")
  , mtbAux2To1568 :: !(StaticBytes 1566)
  } deriving stock (Eq, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric MtBody)

newtype Mt = Mt { unMt :: Checked MtBody }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, StaticByteSized, Binary)
