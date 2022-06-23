{-# LANGUAGE UndecidableInstances #-}

module Scrapti.Classes
  ( Pair (..)
  , Equiv (..)
  , BinaryRep (..)
  , BinaryEncoded (..)
  , BinaryTagged (..)
  , ViaEquiv (..)
  , ViaBoundedEnum (..)
  , ViaBinaryRep (..)
  , ViaBinary (..)
  , ViaBinaryTagged (..)
  ) where

import Data.Default (Default (..))
import Scrapti.Binary (Binary (..), DecodeM, Get, Put, decodeGet)

-- TODO remove in favor of explicit newtypes + equivs
-- | Just a strict tuple
data Pair x y = Pair
  { pairFirst :: !x
  , pairSecond :: !y
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance (Default x, Default y) => Default (Pair x y) where
  def = Pair def def

class Equiv z a | a -> z where
  equivFwd :: z -> a
  equivBwd :: a -> z

class Binary x => BinaryRep x a | a -> x where
  parse :: x -> Either String a
  rep :: a -> x

class BinaryEncoded a where
  decode :: DecodeM a
  encode :: a -> Put

class Binary x => BinaryTagged x a | a -> x where
  getTagged :: x -> Get a
  howTagged :: a -> x
  putTagged :: a -> Put

newtype ViaEquiv a = ViaEquiv { unViaEquiv :: a }

instance (Binary z, Equiv z a) => Binary (ViaEquiv a) where
  get = fmap (ViaEquiv . equivFwd) get
  put = put . equivBwd . unViaEquiv

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

instance Binary a => BinaryEncoded (ViaBinary a) where
  decode = decodeGet (fmap ViaBinary get)
  encode = put . unViaBinary

newtype ViaBinaryTagged a = ViaBinaryTagged { unViaBinaryTagged :: a }

instance BinaryTagged x a => Binary (ViaBinaryTagged a) where
  get = do
    x <- get
    fmap ViaBinaryTagged (getTagged x)
  put (ViaBinaryTagged a) = do
    put (howTagged a)
    putTagged a
