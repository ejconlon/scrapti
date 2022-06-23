{-# LANGUAGE UndecidableInstances #-}

module Scrapti.Classes
  ( BinaryRep (..)
  , BinaryEncoded (..)
  , BinaryTagged (..)
  , ViaBoundedEnum (..)
  , ViaBinaryRep (..)
  , ViaBinary (..)
  , ViaBinaryTagged (..)
  ) where

import Scrapti.Binary (Binary (..), DecodeM, Get, Put, decodeGet)

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
