{-# LANGUAGE UnboxedTuples #-}

module Scrapti.Parser.Nums
  ( Word16LE (..)
  , Int16LE (..)
  ) where

import Data.Default (Default)
import Data.Int (Int16)
import Data.Primitive (Prim)
import Data.Word (Word16)
import Scrapti.Parser.Sizes (ByteSized (..), StaticByteSized (..))

newtype Word16LE = Word16LE { unWord16LE :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)

instance ByteSized Word16LE where
  byteSize = const 2

instance StaticByteSized Word16LE where
  staticByteSize = const 2

newtype Int16LE = Int16LE { unInt16LE :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)

instance ByteSized Int16LE where
  byteSize = const 2

instance StaticByteSized Int16LE where
  staticByteSize = const 2
