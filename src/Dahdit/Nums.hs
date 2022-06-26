{-# LANGUAGE UnboxedTuples #-}

module Dahdit.Nums
  ( Word16LE (..)
  , Int16LE (..)
  ) where

import Dahdit.Sizes (ByteSized (..), StaticByteSized (..))
import Data.Bits (Bits)
import Data.Default (Default)
import Data.Int (Int16)
import Data.Word (Word16)
import Foreign.Storable (Storable)

newtype Word16LE = Word16LE { unWord16LE :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Storable, Default)

instance ByteSized Word16LE where
  byteSize _ = 2

instance StaticByteSized Word16LE where
  staticByteSize _ = 2

newtype Int16LE = Int16LE { unInt16LE :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Storable, Default)

instance ByteSized Int16LE where
  byteSize _ = 2

instance StaticByteSized Int16LE where
  staticByteSize _ = 2
