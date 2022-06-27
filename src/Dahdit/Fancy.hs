module Dahdit.Fancy
  ( TermBytes (..)
  , FixedBytes (..)
  , FixedSeq (..)
  , FixedArray (..)
  ) where

import Control.Monad (unless)
import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get, Put)
import Dahdit.Funs (getShortByteString, getStaticArray, getWord8, putFixedByteString, putShortByteString, putWord8)
import Dahdit.Proxy (Proxy (..))
import Dahdit.Sizes (ByteSized (..), StaticByteSized (..), ViaStaticByteSized (..))
import qualified Data.ByteString.Short as BSS
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Default (Default (..))
import Data.Primitive (Prim)
import Data.Primitive.ByteArray (ByteArray (..), byteArrayFromListN)
import Data.Primitive.PrimArray (PrimArray)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Word (Word8)
import GHC.TypeLits (KnownNat, Nat, natVal)

getUntilNull :: Get (Int, [Word8])
getUntilNull = go 0 [] where
  go !i !racc = do
    w <- getWord8
    if w == 0
      then
        let !acc = reverse racc
        in pure (i, acc)
      else go (i + 1) (w:racc)

mkSBS :: Int -> [Word8] -> ShortByteString
mkSBS n bs = let !(ByteArray ba) = byteArrayFromListN n bs in SBS ba

-- | Bytes terminated with null byte.
-- NOTE: Terminated with TWO null bytes if the string is even length
-- to align to Word16 boundaries, as required for
newtype TermBytes = TermBytes { unTermBytes :: ShortByteString }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance Default TermBytes where
  def = TermBytes BSS.empty

instance ByteSized TermBytes where
  byteSize (TermBytes sbs) =
    let !bc = byteSize sbs + 1
    in if even bc then bc else bc + 1

instance Binary TermBytes where
  get = do
    (!i, acc) <- getUntilNull
    unless (odd i) $ do
      w <- getWord8
      unless (w == 0) (fail "TermBytes missing word pad")
    let !sbs = mkSBS i acc
    pure $! TermBytes sbs

  put (TermBytes sbs) = do
    putShortByteString sbs
    putWord8 0
    unless (odd (BSS.length sbs)) (putWord8 0)

-- | A fixed-length bytestring (truncated or zero-padded on put if length does not match).
newtype FixedBytes (n :: Nat) = FixedBytes { unFixedBytes :: ShortByteString }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)
  deriving (ByteSized) via (ViaStaticByteSized (FixedBytes n))

instance Default (FixedBytes n) where
  def = FixedBytes BSS.empty

instance KnownNat n => StaticByteSized (FixedBytes n) where
  staticByteSize _ = fromInteger (natVal (Proxy :: Proxy n))

instance KnownNat n => Binary (FixedBytes n) where
  get = fmap FixedBytes (getShortByteString (fromInteger (natVal (Proxy :: Proxy n))))
  put fb@(FixedBytes sbs) = putFixedByteString 0 (fromInteger (natVal fb)) sbs

newtype FixedSeq (n :: Nat) a = FixedSeq { unFixedSeq :: Seq a }
  deriving stock (Show)
  deriving newtype (Eq)

instance (KnownNat n, Default a) => Default (FixedSeq n a) where
  def = undefined -- FixedSeq (replicate (fromIntegral (natVal (Proxy :: Proxy n))) def)

instance (KnownNat n, ByteSized a) => ByteSized (FixedSeq n a) where
  byteSize = undefined

instance (KnownNat n, Binary a) => Binary (FixedSeq n a) where
  get = undefined
  put = undefined

newtype FixedArray (n :: Nat) a = FixedArray { unFixedArray :: PrimArray a }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (ByteSized) via (ViaStaticByteSized (FixedArray n a))

instance (KnownNat n, Prim a, Default a) => Default (FixedArray n a) where
  def = undefined -- FixedArray (replicate (fromIntegral (natVal (Proxy :: Proxy n))) def)

instance (KnownNat n, StaticByteSized a) => StaticByteSized (FixedArray n a) where
  staticByteSize = const (fromIntegral (natVal (Proxy :: Proxy n)) * staticByteSize (Proxy :: Proxy a))

instance (KnownNat n, Prim a, StaticByteSized a, Binary a) => Binary (FixedArray n a) where
  get = fmap FixedArray (getStaticArray (fromIntegral (natVal (Proxy :: Proxy n))))
  put (FixedArray arr0) =
    -- TODO put element count in PutFStaticArray + Sequence
    undefined
    -- let !intLen = fromIntegral (natVal (Proxy :: Proxy n))
    --     !v1 = if VP.length v0 == intLen then v0 else VP.take intLen v0
    -- in putVec v1
