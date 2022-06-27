module Dahdit.Example where

import Dahdit.Prelude (Binary, ByteSized, Generic, StaticByteSized, ViaGeneric (..), ViaStaticGeneric (..), Word16LE,
                       Word8)

data DynFoo = DynFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, Binary) via (ViaGeneric DynFoo)

data StaFoo = StaFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric StaFoo)
