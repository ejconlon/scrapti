module Dahdit.Fancy
  ( TermBytes (..)
  , StaticBytes (..)
  , StaticSeq (..)
  , StaticArray (..)
  , BoolByte (..)
  ) where

import Control.Monad (unless)
import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get)
import Dahdit.Funs (getByteString, getStaticArray, getStaticSeq, getWord8, putByteString, putFixedString, putWord8,
                    unsafePutStaticArrayN, unsafePutStaticSeqN)
import Dahdit.Proxy (Proxy (..))
import Dahdit.Sizes (ByteSized (..), StaticByteSized (..), ViaStaticByteSized (..), staticByteSizeFoldable)
import qualified Data.ByteString.Short as BSS
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Default (Default (..))
import Data.Primitive (Prim)
import Data.Primitive.ByteArray (ByteArray (..), byteArrayFromListN)
import Data.Primitive.PrimArray (PrimArray, replicatePrimArray)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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
-- to align to Word16 boundaries, as required for RIFF format, for example.
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
    putByteString sbs
    putWord8 0
    unless (odd (BSS.length sbs)) (putWord8 0)

-- | A fixed-length bytestring (truncated or zero-padded on put if length does not match).
newtype StaticBytes (n :: Nat) = StaticBytes { unStaticBytes :: ShortByteString }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)
  deriving (ByteSized) via (ViaStaticByteSized (StaticBytes n))

instance Default (StaticBytes n) where
  def = StaticBytes BSS.empty

instance KnownNat n => StaticByteSized (StaticBytes n) where
  staticByteSize _ = fromInteger (natVal (Proxy :: Proxy n))

instance KnownNat n => Binary (StaticBytes n) where
  get = fmap StaticBytes (getByteString (fromInteger (natVal (Proxy :: Proxy n))))
  put fb@(StaticBytes sbs) = putFixedString 0 (fromInteger (natVal fb)) sbs

newtype StaticSeq (n :: Nat) a = StaticSeq { unStaticSeq :: Seq a }
  deriving stock (Show)
  deriving newtype (Eq, Functor, Foldable)
  deriving (ByteSized) via (ViaStaticByteSized (StaticSeq n a))

instance (KnownNat n, Default a) => Default (StaticSeq n a) where
  def = StaticSeq (Seq.replicate (fromIntegral (natVal (Proxy :: Proxy n))) def)

instance (StaticByteSized a) => StaticByteSized (StaticSeq n a) where
  staticByteSize _ = staticByteSizeFoldable (Proxy :: Proxy a)

instance (KnownNat n, Binary a, StaticByteSized a, Default a) => Binary (StaticSeq n a) where
  get = fmap StaticSeq (getStaticSeq (fromIntegral (natVal (Proxy :: Proxy n))) get)
  put = unsafePutStaticSeqN (fromIntegral (natVal (Proxy :: Proxy n))) (Just def) put . unStaticSeq
  -- put (StaticSeq s) =
  --   let !n = fromIntegral (natVal (Proxy :: Proxy n))
  --       !e = fromIntegral (Seq.length s)
  --   in if n > e
  --     then error ("Wrong number of elements to put static seq (have " ++ show (unElementCount e) ++ ", need " ++ show (unElementCount n) ++ ")")
  --     else unsafePutStaticSeqN n put s

newtype StaticArray (n :: Nat) a = StaticArray { unStaticArray :: PrimArray a }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (ByteSized) via (ViaStaticByteSized (StaticArray n a))

instance (KnownNat n, Prim a, Default a) => Default (StaticArray n a) where
  def = StaticArray (replicatePrimArray (fromIntegral (natVal (Proxy :: Proxy n))) def)

instance (KnownNat n, StaticByteSized a) => StaticByteSized (StaticArray n a) where
  staticByteSize _ = fromIntegral (natVal (Proxy :: Proxy n)) * staticByteSize (Proxy :: Proxy a)

instance (KnownNat n, Prim a, StaticByteSized a, Default a) => Binary (StaticArray n a) where
  get = fmap StaticArray (getStaticArray (fromIntegral (natVal (Proxy :: Proxy n))))
  put = unsafePutStaticArrayN (fromIntegral (natVal (Proxy :: Proxy n))) (Just def) . unStaticArray
    -- let !n = fromIntegral (natVal (Proxy :: Proxy n))
    --     !e = fromIntegral (sizeofPrimArray a)
    -- in if n > e
    --   then error ("Wrong number of elements to put static array (have " ++ show (unElementCount e) ++ ", need " ++ show (unElementCount n) ++ ")")
    --   else putStaticArrayN n a

newtype BoolByte = BoolByte { unBoolByte :: Bool }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (ByteSized) via (ViaStaticByteSized BoolByte)

instance Default BoolByte where
  def = BoolByte False

instance StaticByteSized BoolByte where
  staticByteSize _ = 1

instance Binary BoolByte where
  get = fmap (BoolByte . (/= 0)) getWord8
  put (BoolByte b) = putWord8 (if b then 1 else 0)
