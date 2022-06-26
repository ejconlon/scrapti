{-# LANGUAGE MagicHash #-}

module Dahdit.Nums
  ( Word16LE (..)
  , Int16LE (..)
  ) where

import Dahdit.Sizes (ByteSized (..), StaticByteSized (..))
import Data.Bits (Bits)
import Data.Default (Default)
import Data.Int (Int16)
import Data.Primitive.Types (Prim (..))
import Data.Word (Word16)

newtype Word16LE = Word16LE { unWord16LE :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default)

instance ByteSized Word16LE where
  byteSize _ = 2

instance StaticByteSized Word16LE where
  staticByteSize _ = 2

-- TODO figure out how to implement this...
instance Prim Word16LE where
  sizeOf# _ = 2#
  alignment# _ = undefined
  indexByteArray# _ _ = undefined
  readByteArray# _ _ _ = undefined
  writeByteArray# _ _ _ _ = undefined
  setByteArray# _ _ _ _ = undefined
  indexOffAddr# _ _ = undefined
  readOffAddr# _ _ _ = undefined
  writeOffAddr# _ _ _ _ = undefined
  setOffAddr# _ _ _ _ = undefined

-- TODO move to the prim instance
-- readWord16LE :: ByteArray -> Int -> Word16LE
-- readWord16LE arr pos =
--   let !b0 = readWord8 arr pos
--       !b1 = readWord8 arr (pos + 1)
--       !w = (fromIntegral b1 `unsafeShiftL` 8) .|. fromIntegral b0
--   in Word16LE w

newtype Int16LE = Int16LE { unInt16LE :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default)

instance ByteSized Int16LE where
  byteSize _ = 2

instance StaticByteSized Int16LE where
  staticByteSize _ = 2

instance Prim Int16LE where
  sizeOf# _ = 2#
  alignment# _ = undefined
  indexByteArray# _ _ = undefined
  readByteArray# _ _ _ = undefined
  writeByteArray# _ _ _ _ = undefined
  setByteArray# _ _ _ _ = undefined
  indexOffAddr# _ _ = undefined
  readOffAddr# _ _ _ = undefined
  writeOffAddr# _ _ _ _ = undefined
  setOffAddr# _ _ _ _ = undefined
