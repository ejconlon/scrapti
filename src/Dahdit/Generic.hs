{-# LANGUAGE UndecidableInstances #-}

module Dahdit.Generic
  ( ViaGeneric (..)
  , ViaStaticGeneric (..)
  ) where

import Control.Applicative (liftA2)
import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get, Put)
import Dahdit.Funs (putStaticHint)
import Dahdit.Proxy (Proxy (..))
import Dahdit.Sizes (ByteCount, ByteSized (..), StaticByteSized (..))
import Data.Kind (Type)
import GHC.Generics ((:*:) (..), Generic (..), K1 (..), M1 (..), U1 (..), V1)

-- | Use: deriving (ByteSized, Binary) via (ViaGeneric Foo)
newtype ViaGeneric a = ViaGeneric { unViaGeneric :: a }

-- | Use: deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Foo)
newtype ViaStaticGeneric a = ViaStaticGeneric { unViaStaticGeneric :: a }

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

instance (Generic t, GByteSized (Rep t)) => ByteSized (ViaStaticGeneric t) where
  byteSize = gbyteSize . from . unViaStaticGeneric

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

instance (Generic t, GStaticByteSized (Rep t)) => StaticByteSized (ViaStaticGeneric t) where
  staticByteSize _ = gstaticByteSize (Proxy :: Proxy (Rep t))

-- Binary:

class GByteSized f => GBinary (f :: Type -> Type) where
  gget :: Get (f a)
  gput :: f a -> Put

instance GBinary U1 where
  gget = pure U1
  gput _ = pure ()

instance (GBinary a, GBinary b) => GBinary (a :*: b) where
  gget = liftA2 (:*:) gget gget
  gput (x :*: y) = gput x *> gput y

instance GBinary a => GBinary (M1 i c a) where
  gget = fmap M1 gget
  gput = gput . unM1

instance Binary a => GBinary (K1 i a) where
  gget = fmap K1 get
  gput = put . unK1

instance (Generic t, GBinary (Rep t)) => Binary (ViaGeneric t) where
  get = fmap (ViaGeneric . to) gget
  put = gput . from . unViaGeneric

instance (Generic t, GStaticByteSized (Rep t), GBinary (Rep t)) => Binary (ViaStaticGeneric t) where
  get = fmap (ViaStaticGeneric . to) gget
  put = putStaticHint (gput . from . unViaStaticGeneric)
