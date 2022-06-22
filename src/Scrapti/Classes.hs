{-# LANGUAGE UndecidableInstances #-}

module Scrapti.Classes where

import Data.Binary (Binary (..), Put)
import Scrapti.Binary (DecodeT, decodeGet)

class Binary x => BinaryRep x a | a -> x where
  parse :: x -> Either String a
  rep :: a -> x

class BinaryEncoder a where
  encode :: a -> Put

class (BinaryEncoder a, Monad m) => BinaryDecoder m a where
  decode :: DecodeT m a

class DefaultWith e a | a -> e where
  defWith :: e -> a

newtype ViaBoundedEnum x a = ViaBoundedEnum { unViaBoundedEnum :: a }

instance (Binary x, Integral x, Bounded a, Enum a) => BinaryRep x (ViaBoundedEnum x a) where
  parse x =
    let i = fromIntegral x
    in if i < fromEnum (minBound :: a) || i > fromEnum (maxBound :: a)
      then Left ("invalid enum value: " ++ show i)
      else Right (ViaBoundedEnum (toEnum i))
  rep = fromIntegral . fromEnum . unViaBoundedEnum

newtype ViaBinaryRep a = ViaBinaryRep { unViaBinaryRep :: a }

instance BinaryRep x a => Binary (ViaBinaryRep a) where
  get = get >>= either fail (pure . ViaBinaryRep) . parse
  put = put . rep . unViaBinaryRep

newtype ViaBinary a = ViaBinary { unViaBinary :: a }

instance Binary a => BinaryEncoder (ViaBinary a) where
  encode = put . unViaBinary

instance (Binary a, Monad m) => BinaryDecoder m (ViaBinary a) where
  decode = decodeGet (fmap ViaBinary get)
