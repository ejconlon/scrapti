{-# LANGUAGE UndecidableInstances #-}

module Scrapti.Parser.Via
  ( ViaGeneric (..)
  -- , ViaStaticGeneric (..)
  ) where

import GHC.Generics (Generic (..), V1, U1, type (:*:) (..), K1 (..), M1 (..))
import Scrapti.Parser.Sizes (ByteCount, ByteSized (..), StaticByteSized (..))
import Data.Proxy (Proxy (..))
import Data.Kind (Type)

newtype ViaGeneric a = ViaGeneric { unViaGeneric :: a }

-- ByteSized:

class GByteSized f where
  gbyteSize :: f a -> ByteCount

-- Void
instance GByteSized V1 where
  gbyteSize _ = 0

-- Unit
instance GByteSized U1 where
  gbyteSize _ = 0

-- Product
instance (GByteSized a, GByteSized b) => GByteSized (a :*: b) where
  gbyteSize (x :*: y) = gbyteSize x + gbyteSize y

-- Metadata
instance GByteSized a => GByteSized (M1 i c a) where
  gbyteSize = gbyteSize . unM1

-- Field
instance ByteSized a => GByteSized (K1 i a) where
  gbyteSize = byteSize . unK1

instance (Generic t, GByteSized (Rep t)) => ByteSized (ViaGeneric t) where
  byteSize = gbyteSize . from . unViaGeneric

-- StaticByteSized:

class GByteSized f => GStaticByteSized (f :: Type -> Type) where
  gstaticByteSize :: Proxy f -> ByteCount

instance GStaticByteSized V1 where
  gstaticByteSize _ = 0

instance GStaticByteSized U1 where
  gstaticByteSize _ = 0

instance (GStaticByteSized a, GStaticByteSized b) => GStaticByteSized (a :*: b) where
  gstaticByteSize _ = gstaticByteSize (Proxy :: Proxy a) + gstaticByteSize (Proxy :: Proxy b)

instance GStaticByteSized a => GStaticByteSized (M1 i c a) where
  gstaticByteSize _ = gstaticByteSize (Proxy :: Proxy a)

instance StaticByteSized a => GStaticByteSized (K1 i a) where
  gstaticByteSize _ = staticByteSize (Proxy :: Proxy a)

instance (Generic t, GStaticByteSized (Rep t)) => StaticByteSized (ViaGeneric t) where
  staticByteSize _ = gstaticByteSize (Proxy :: Proxy (Rep t))
