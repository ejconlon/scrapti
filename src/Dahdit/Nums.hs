{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- The derived instances here work for little-endian, which covers intel/arm.
-- Custom instances will have to be provided for big endian or to support portability.
-- The Prim implementations here are probably extremely bad - I'm just trying to make it work.
-- I used this example: https://hackage.haskell.org/package/prim-instances-0.2/docs/src/Data.Primitive.Instances.html
-- Also it's proabably not as efficient to derive implementations but it is what it is.
module Dahdit.Nums
  ( Word16LE (..)
  , Int16LE (..)
  , LiftedPrim (..)
  ) where

import Control.Monad.Primitive (PrimMonad (..))
import Dahdit.Sizes (ByteSized (..), StaticByteSized (..))
import Data.Bits (Bits (..))
import Data.Default (Default)
import Data.Int (Int16, Int8)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, indexByteArray, writeByteArray)
import Data.Primitive.Types (Prim (..))
import Data.Word (Word16, Word8)
-- import GHC.Exts ((*#), (+#))

newtype ViaFromIntegral x y = ViaFromIntegral { unViaFromIntegral :: y }

-- TODO finish this
instance (Num x, Integral x, Prim x, Num y, Integral y) => Prim (ViaFromIntegral x y) where
  sizeOf# _ = sizeOf# (undefined :: x)
  alignment# _ = alignment# (undefined :: x)
  indexByteArray# arr# i# = ViaFromIntegral (fromIntegral (indexByteArray# arr# i# :: x))
  readByteArray# arr# i# s0 = let !(# s1#, x :: x #) = readByteArray# arr# i# s0 in (# s1#, ViaFromIntegral (fromIntegral x) #)
  writeByteArray# arr# i# y = let !x = fromIntegral (unViaFromIntegral y) :: x in writeByteArray# arr# i# x
  setByteArray# _ _ _ _ = undefined
  indexOffAddr# _ _ = undefined
  readOffAddr# _ _ _ = undefined
  writeOffAddr# _ _ _ _ = undefined
  setOffAddr# _ _ _ _ = undefined

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

-- instance Prim Word16LE where
--   sizeOf# _ = 2# *# sizeOf# (undefined :: Word8)
--   alignment# _ = alignment# (undefined :: Word8)
--   indexByteArray# arr# i# =
--     let !b0 = indexByteArray# arr# (2# *# i#)
--         !b1 = indexByteArray# arr# (2# *# i# +# 1#)
--     in mkWord16LE b0 b1
--   readByteArray# arr# i# s0 =
--     case readByteArray# arr# (2# *# i#) s0 of
--       (# s1#, x #) -> case readByteArray# arr# (2# *# i# +# 1#) s1# of
--         (# s2#, y #) -> (# s2#, mkWord16LE x y #)
--   writeByteArray# arr# i# w =
--     let !(# b0, b1 #) = unMkWord16LE w
--     in \s0 -> case writeByteArray# arr# (2# *# i#) b0 s0 of
--       s1 -> case writeByteArray# arr# (2# *# i# +# 1#) b1 s1 of
--         s2 -> s2
--   setByteArray# = defaultSetByteArray#
--   indexOffAddr# _ _ = undefined
--   readOffAddr# _ _ _ = undefined
--   writeOffAddr# _ _ _ _ = undefined
--   setOffAddr# = defaultSetOffAddr#

newtype Int16LE = Int16LE { unInt16LE :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default, Prim)
  deriving (LiftedPrim) via (ViaFromIntegral Word16LE Int16LE)

instance ByteSized Int16LE where
  byteSize _ = 2

instance StaticByteSized Int16LE where
  staticByteSize _ = 2
