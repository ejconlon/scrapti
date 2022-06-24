module Scrapti.Parser.Funs
  ( getWord8
  , getInt8
  , getWord16LE
  , getInt16LE
  , getByteString
  , getSkip
  , getExact
  , getWithin
  , getSeq
  , getFixedSeq
  , getFixedVector
  , putWord8
  , putInt8
  , putWord16LE
  , putInt16LE
  , putByteString
  , putSeq
  , putFixedSeq
  , putFixedVector
  ) where

import Control.Monad.Free.Church (F (..))
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Sequence (Seq (..))
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8)
import Scrapti.Parser.Free (Get (..), GetF (..), GetFixedSeqF (..), GetFixedVectorF (..), Put, PutF (..),
                            PutFixedSeqF (..), PutFixedVectorF (..), PutM (..), ScopeMode (..))
import Scrapti.Parser.Nums (Int16LE, Word16LE)
import Scrapti.Parser.Sizes (ByteCount, ElementCount, StaticByteSized)

getWord8 :: Get Word8
getWord8 = Get (F (\x y -> y (GetFWord8 x)))

getInt8 :: Get Int8
getInt8 = Get (F (\x y -> y (GetFInt8 x)))

getWord16LE :: Get Word16LE
getWord16LE = Get (F (\x y -> y (GetFWord16LE x)))

getInt16LE :: Get Int16LE
getInt16LE = Get (F (\x y -> y (GetFInt16LE x)))

getByteString :: ByteCount -> Get ByteString
getByteString bc = Get (F (\x y -> y (GetFByteString bc x)))

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
getFixedSeq :: StaticByteSized a => ElementCount -> Get a -> Get (Seq a)
getFixedSeq n g = Get (F (\x y -> y (GetFFixedSeq (GetFixedSeqF n g x))))

-- | Get Vector of statically-sized elements
getFixedVector :: (StaticByteSized a, Prim a) => ElementCount -> Get a -> Get (VP.Vector a)
getFixedVector n g = Get (F (\x y -> y (GetFFixedVector (GetFixedVectorF n g x))))

putWord8 :: Word8 -> Put
putWord8 d = PutM (F (\x y -> y (PutFWord8 d (x ()))))

putInt8 :: Int8 -> Put
putInt8 d = PutM (F (\x y -> y (PutFInt8 d (x ()))))

putWord16LE :: Word16LE -> Put
putWord16LE d = PutM (F (\x y -> y (PutFWord16LE d (x ()))))

putInt16LE :: Int16LE -> Put
putInt16LE d = PutM (F (\x y -> y (PutFInt16LE d (x ()))))

putByteString :: ByteString -> Put
putByteString bs = PutM (F (\x y -> y (PutFByteString bs (x ()))))

-- | Put Seq of dynamically-sized elements
putSeq :: (a -> Put) -> Seq a -> Put
putSeq = traverse_

-- | Put Seq of statically-sized elements
putFixedSeq :: StaticByteSized a => (a -> Put) -> Seq a -> Put
putFixedSeq p s = PutM (F (\x y -> y (PutFFixedSeq (PutFixedSeqF s p (x ())))))

-- | Put Vector of statically-sized elements
putFixedVector :: (StaticByteSized a, Prim a) => (a -> Put) -> VP.Vector a -> Put
putFixedVector p v = PutM (F (\x y -> y (PutFFixedVector (PutFixedVectorF v p (x ())))))
