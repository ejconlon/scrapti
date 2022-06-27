{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- The derived instances here work for little-endian, which covers intel/arm.
-- Custom instances will have to be provided for big endian or to support portability.
module Dahdit.Nums
  ( Word16LE (..)
  , Int16LE (..)
  , Word32LE (..)
  , Int32LE (..)
  , FloatLE (..)
  , LiftedPrim (..)
  ) where

import Control.Monad.Primitive (PrimMonad (..))
import Dahdit.Sizes (ByteSized (..), StaticByteSized (..))
import Data.Bits (Bits (..))
import Data.Default (Default)
import Data.Int (Int16, Int32, Int8)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, indexByteArray, writeByteArray)
import Data.Primitive.Types (Prim (..))
import Data.Word (Word16, Word32, Word8)
import GHC.Float (castFloatToWord32, castWord32ToFloat)

newtype ViaFromIntegral x y = ViaFromIntegral { unViaFromIntegral :: y }

-- Indices here are in bytes, not elements
class LiftedPrim w where
  indexByteArrayLifted :: ByteArray -> Int -> w
  writeByteArrayLifted :: PrimMonad m => w -> MutableByteArray (PrimState m) -> Int -> m ()

instance LiftedPrim Word8 where
  indexByteArrayLifted = indexByteArray
  writeByteArrayLifted val arr pos = writeByteArray arr pos val

instance LiftedPrim Int8 where
  indexByteArrayLifted = indexByteArray
  writeByteArrayLifted val arr pos = writeByteArray arr pos val

instance (Num x, Integral x, LiftedPrim x, Num y, Integral y) => LiftedPrim (ViaFromIntegral x y) where
  indexByteArrayLifted arr pos = ViaFromIntegral (fromIntegral (indexByteArrayLifted arr pos :: x))
  writeByteArrayLifted val arr pos = let !x = fromIntegral (unViaFromIntegral val) :: x in writeByteArrayLifted x arr pos

newtype Word16LE = Word16LE { unWord16LE :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default, Prim)

instance ByteSized Word16LE where
  byteSize _ = 2

instance StaticByteSized Word16LE where
  staticByteSize _ = 2

mkWord16LE :: Word8 -> Word8 -> Word16LE
mkWord16LE b0 b1 =
  let !w = (fromIntegral b1 `unsafeShiftL` 8) .|. fromIntegral b0
  in Word16LE w

unMkWord16LE :: Word16LE -> (Word8, Word8)
unMkWord16LE (Word16LE w) =
  let !b0 = fromIntegral w
      !b1 = fromIntegral (w `shiftR` 8)
  in (b0, b1)

instance LiftedPrim Word16LE where
  indexByteArrayLifted arr pos =
    let !b0 = indexByteArray arr pos
        !b1 = indexByteArray arr (pos + 1)
    in mkWord16LE b0 b1

  writeByteArrayLifted w arr pos =
    let !(b0, b1) = unMkWord16LE w
    in writeByteArray arr pos b0 *> writeByteArray arr (pos + 1) b1

newtype Int16LE = Int16LE { unInt16LE :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default, Prim)
  deriving (LiftedPrim) via (ViaFromIntegral Word16LE Int16LE)

instance ByteSized Int16LE where
  byteSize _ = 2

instance StaticByteSized Int16LE where
  staticByteSize _ = 2

newtype Word32LE = Word32LE { unWord32LE :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default, Prim)

instance ByteSized Word32LE where
  byteSize _ = 4

instance StaticByteSized Word32LE where
  staticByteSize _ = 4

mkWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32LE
mkWord32LE b0 b1 b2 b3 =
  let !w = (fromIntegral b3 `unsafeShiftL` 24) .|. (fromIntegral b2 `unsafeShiftL` 16) .|.
            (fromIntegral b1 `unsafeShiftL` 8) .|. fromIntegral b0
  in Word32LE w

unMkWord32LE :: Word32LE -> (Word8, Word8, Word8, Word8)
unMkWord32LE (Word32LE w) =
  let !b0 = fromIntegral w
      !b1 = fromIntegral (w `shiftR` 8)
      !b2 = fromIntegral (w `shiftR` 16)
      !b3 = fromIntegral (w `shiftR` 24)
  in (b0, b1, b2, b3)

instance LiftedPrim Word32LE where
  indexByteArrayLifted arr pos =
    let !b0 = indexByteArray arr pos
        !b1 = indexByteArray arr (pos + 1)
        !b2 = indexByteArray arr (pos + 2)
        !b3 = indexByteArray arr (pos + 3)
    in mkWord32LE b0 b1 b2 b3

  writeByteArrayLifted w arr pos = do
    let !(b0, b1, b2, b3) = unMkWord32LE w
    writeByteArray arr pos b0
    writeByteArray arr (pos + 1) b1
    writeByteArray arr (pos + 2) b2
    writeByteArray arr (pos + 3) b3

newtype Int32LE = Int32LE { unInt32LE :: Int32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default, Prim)
  deriving (LiftedPrim) via (ViaFromIntegral Word32LE Int32LE)

instance ByteSized Int32LE where
  byteSize _ = 4

instance StaticByteSized Int32LE where
  staticByteSize _ = 4

newtype FloatLE = FloatLE { unFloatLE :: Float }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default, Prim)

instance ByteSized FloatLE where
  byteSize _ = 4

instance StaticByteSized FloatLE where
  staticByteSize _ = 4

mkFloatLE :: Word8 -> Word8 -> Word8 -> Word8 -> FloatLE
mkFloatLE b0 b1 b2 b3 =
  let !(Word32LE w) = mkWord32LE b0 b1 b2 b3
      !f = castWord32ToFloat w
  in FloatLE f

unMkFloatLE :: FloatLE -> (Word8, Word8, Word8, Word8)
unMkFloatLE (FloatLE f) =
  let !w = castFloatToWord32 f
  in unMkWord32LE (Word32LE w)

instance LiftedPrim FloatLE where
  indexByteArrayLifted arr pos =
    let !b0 = indexByteArray arr pos
        !b1 = indexByteArray arr (pos + 1)
        !b2 = indexByteArray arr (pos + 2)
        !b3 = indexByteArray arr (pos + 3)
    in mkFloatLE b0 b1 b2 b3

  writeByteArrayLifted f arr pos = do
    let !(b0, b1, b2, b3) = unMkFloatLE f
    writeByteArray arr pos b0
    writeByteArray arr (pos + 1) b1
    writeByteArray arr (pos + 2) b2
    writeByteArray arr (pos + 3) b3
