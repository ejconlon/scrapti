module Scrapti.Tracker.Mt
  ( Mt (..)
  ) where

import Dahdit (Binary, ByteSized, StaticByteSized, ViaStaticGeneric (..), Word8)
import GHC.Generics (Generic)

-- TODO real defn
data Mt = Mt !Word8 !Word8
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Mt)
