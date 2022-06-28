{-# LANGUAGE UndecidableInstances #-}

module Dahdit.BinaryRep
  ( BinaryRep (..)
  , ViaBoundedEnum (..)
  , ViaBinaryRep (..)
  ) where

import Dahdit.Binary (Binary (..))
import Dahdit.Proxy (Proxy (..))
import Dahdit.Sizes (ByteSized (..), StaticByteSized (..))

class Binary x => BinaryRep x a | a -> x where
  fromBinaryRep :: x -> Either String a
  toBinaryRep :: a -> x

newtype ViaBoundedEnum x a = ViaBoundedEnum { unViaBoundedEnum :: a }

instance (Binary x, Integral x, Bounded a, Enum a) => BinaryRep x (ViaBoundedEnum x a) where
  fromBinaryRep x =
    let i = fromIntegral x
    in if i < fromEnum (minBound :: a) || i > fromEnum (maxBound :: a)
      then Left ("Invalid enum value: " ++ show i)
      else Right (ViaBoundedEnum (toEnum i))
  toBinaryRep = fromIntegral . fromEnum . unViaBoundedEnum

newtype ViaBinaryRep a = ViaBinaryRep { unViaBinaryRep :: a }

instance (ByteSized x, BinaryRep x a) => ByteSized (ViaBinaryRep a) where
  byteSize = byteSize . toBinaryRep . unViaBinaryRep

instance (StaticByteSized x, BinaryRep x a) => StaticByteSized (ViaBinaryRep a) where
  staticByteSize _ = staticByteSize (Proxy :: Proxy x)

instance (BinaryRep x a, Binary x) => Binary (ViaBinaryRep a) where
  get = get >>= either fail (pure . ViaBinaryRep) . fromBinaryRep
  put = put . toBinaryRep . unViaBinaryRep
