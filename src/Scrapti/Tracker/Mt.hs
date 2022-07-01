module Scrapti.Tracker.Mt
  ( MtBody (..)
  , Mt (..)
  ) where

import Dahdit (Binary, ByteSized, ExactBytes, StaticByteSized, StaticBytes, ViaStaticGeneric (..))
import GHC.Generics (Generic)
import Scrapti.Tracker.Checked (Checked (..))

data MtBody = MtBody
  { mtbFileType :: !(ExactBytes "MT")
  , mtbAux2To1792 :: !(StaticBytes 1790)
  } deriving stock (Eq, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric MtBody)

newtype Mt = Mt { unMt :: Checked MtBody }
  deriving stock (Show)
  deriving newtype (Eq, ByteSized, StaticByteSized, Binary)
