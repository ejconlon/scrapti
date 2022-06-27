module Dahdit.Funs
  ( getWord8
  , getInt8
  , getWord16LE
  , getInt16LE
  , getShortByteString
  , getSkip
  , getExact
  , getWithin
  , getSeq
  , getStaticSeq
  , getStaticArray
  , putWord8
  , putInt8
  , putWord16LE
  , putInt16LE
  , putShortByteString
  , putSeq
  , putStaticSeq
  , putStaticArray
  , putStaticHint
  ) where

import Control.Monad.Free.Church (F (..))
import Dahdit.Free (Get (..), GetF (..), GetStaticArrayF (..), GetStaticSeqF (..), Put, PutF (..), PutM (..),
                    PutStaticArrayF (..), PutStaticSeqF (..), ScopeMode (..))
import Dahdit.Nums (Int16LE, Word16LE)
import Dahdit.Proxy (Proxy (..), proxyForFun)
import Dahdit.Sizes (ByteCount, ElementCount, StaticByteSized (..))
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (traverse_)
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Primitive.PrimArray (PrimArray)
import Data.Sequence (Seq (..))
import Data.Word (Word8)

getWord8 :: Get Word8
getWord8 = Get (F (\x y -> y (GetFWord8 x)))

getInt8 :: Get Int8
getInt8 = Get (F (\x y -> y (GetFInt8 x)))

getWord16LE :: Get Word16LE
getWord16LE = Get (F (\x y -> y (GetFWord16LE x)))

getInt16LE :: Get Int16LE
getInt16LE = Get (F (\x y -> y (GetFInt16LE x)))

getShortByteString :: ByteCount -> Get ShortByteString
getShortByteString bc = Get (F (\x y -> y (GetFShortByteString bc x)))

getSkip :: ByteCount -> Get ()
getSkip bc = Get (F (\x y -> y (GetFSkip bc (x ()))))

getExact :: ByteCount -> Get a -> Get a
getExact bc (Get (F w)) = Get (F (\x y -> y (GetFScope ScopeModeExact bc (w x y))))

getWithin :: ByteCount -> Get a -> Get a
getWithin bc (Get (F w)) = Get (F (\x y -> y (GetFScope ScopeModeWithin bc (w x y))))

-- | Get Seq of dynamically-sized elements
getSeq :: ElementCount -> Get a -> Get (Seq a)
getSeq n g = go Empty 0 where
  go !acc i =
    if i == n
      then pure acc
      else do
        x <- g
        x `seq` go (acc :|> x) (i + 1)

-- | Get Seq of statically-sized elements
getStaticSeq :: (StaticByteSized a) => ElementCount -> Get a -> Get (Seq a)
getStaticSeq n g = Get (F (\x y -> y (GetFStaticSeq (GetStaticSeqF n g x))))

-- | Get PrimArray of statically-sized elements
getStaticArray :: (StaticByteSized a, Prim a) => ElementCount -> Get (PrimArray a)
getStaticArray n = Get (F (\x y -> y (GetFStaticArray (GetStaticArrayF n (Proxy :: Proxy a) x))))

putWord8 :: Word8 -> Put
putWord8 d = PutM (F (\x y -> y (PutFWord8 d (x ()))))

putInt8 :: Int8 -> Put
putInt8 d = PutM (F (\x y -> y (PutFInt8 d (x ()))))

putWord16LE :: Word16LE -> Put
putWord16LE d = PutM (F (\x y -> y (PutFWord16LE d (x ()))))

putInt16LE :: Int16LE -> Put
putInt16LE d = PutM (F (\x y -> y (PutFInt16LE d (x ()))))

putShortByteString :: ShortByteString -> Put
putShortByteString bs = PutM (F (\x y -> y (PutFShortByteString bs (x ()))))

-- | Put Seq of dynamically-sized elements
putSeq :: (a -> Put) -> Seq a -> Put
putSeq = traverse_

-- | Put Seq of statically-sized elements
putStaticSeq :: StaticByteSized a => (a -> Put) -> Seq a -> Put
putStaticSeq p s = PutM (F (\x y -> y (PutFStaticSeq (PutStaticSeqF s p (x ())))))

-- | Put Array of statically-sized elements
putStaticArray :: (StaticByteSized a, Prim a) => PrimArray a -> Put
putStaticArray v = PutM (F (\x y -> y (PutFStaticArray (PutStaticArrayF v (x ())))))

putStaticHint :: StaticByteSized a => (a -> Put) -> a -> Put
putStaticHint p =
  let !bc = staticByteSize (proxyForFun p)
  in \a -> PutM (F (\x y -> y (PutFStaticHint bc (x ())))) *> p a
