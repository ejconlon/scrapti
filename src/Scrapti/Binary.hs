module Scrapti.Binary
  ( QuietArray (..)
  , DepBinary (..)
  , DepPair (..)
  ) where

import Dahdit (Binary (..), ByteSized (..), Get, Put)
import Data.Default (Default (..))
import Data.Primitive (Prim)
import Data.Primitive.PrimArray (PrimArray, emptyPrimArray, sizeofPrimArray)

newtype QuietArray a = QuietArray { unQuietArray :: PrimArray a }
  deriving newtype (Eq, ByteSized)

instance Prim a => Show (QuietArray a) where
  show (QuietArray arr) = "QuietArray{" ++ show (sizeofPrimArray arr) ++ "}"

instance Default (QuietArray a) where
  def = QuietArray emptyPrimArray

class ByteSized b => DepBinary a b | b -> a where
  getDep :: a -> Get b
  putDep :: a -> b -> Put

data DepPair a b = DepPair
  { depPairKey :: !a
  , depPairVal :: !b
  } deriving stock (Eq, Show)

instance (ByteSized a, ByteSized b) => ByteSized (DepPair a b) where
  byteSize (DepPair a b) = byteSize a + byteSize b

instance (Binary a, DepBinary a b) => Binary (DepPair a b) where
  get = do
    a <- get
    b <- getDep a
    pure $! DepPair a b
  put (DepPair a b) = do
    put a
    putDep a b
