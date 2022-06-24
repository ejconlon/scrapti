module Scrapti.Parser.Free
  ( GetFixedSeqF (..)
  , GetFixedVectorF (..)
  , ScopeMode (..)
  , GetF (..)
  , Get (..)
  , PutFixedSeqF (..)
  , PutFixedVectorF (..)
  , PutF (..)
  , PutM (..)
  , Put
  ) where

import Control.Monad.Free.Church (F (..))
import Data.ByteString (ByteString)
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Sequence (Seq)
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8)
import Scrapti.Parser.Nums (Int16LE, Word16LE)
import Scrapti.Parser.Sizes (ByteCount, ElementCount, StaticByteSized)

data GetFixedSeqF a where
  GetFixedSeqF :: StaticByteSized z => !ElementCount -> Get z -> (Seq z -> a) -> GetFixedSeqF a

instance Functor GetFixedSeqF where
  fmap f (GetFixedSeqF ec g k) = GetFixedSeqF ec g (f . k)

data GetFixedVectorF a where
  GetFixedVectorF :: (StaticByteSized z, Prim z) => !ElementCount -> Get z -> (VP.Vector z -> a) -> GetFixedVectorF a

instance Functor GetFixedVectorF where
  fmap f (GetFixedVectorF ec g k) = GetFixedVectorF ec g (f . k)

data ScopeMode =
    ScopeModeExact
  | ScopeModeWithin
  deriving stock (Eq, Show)

data GetF a =
    GetFWord8 (Word8 -> a)
  | GetFInt8 (Int8 -> a)
  | GetFWord16LE (Word16LE -> a)
  | GetFInt16LE (Int16LE -> a)
  | GetFByteString !ByteCount (ByteString -> a)
  | GetFFixedSeq !(GetFixedSeqF a)
  | GetFFixedVector !(GetFixedVectorF a)
  | GetFScope !ScopeMode !ByteCount a
  | GetFSkip !ByteCount a
  | GetFFail !String
  deriving stock (Functor)

newtype Get a = Get { unGet :: F GetF a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail Get where
  fail msg = Get (F (\_ y -> y (GetFFail msg)))

data PutFixedSeqF a where
  PutFixedSeqF :: StaticByteSized z => !(Seq z) -> (z -> Put) -> a -> PutFixedSeqF a

instance Functor PutFixedSeqF where
  fmap f (PutFixedSeqF s p k) = PutFixedSeqF s p (f k)

data PutFixedVectorF a where
  PutFixedVectorF :: (StaticByteSized z, Prim z) => !(VP.Vector z) -> (z -> Put) -> a -> PutFixedVectorF a

instance Functor PutFixedVectorF where
  fmap f (PutFixedVectorF n g k) = PutFixedVectorF n g (f k)

data PutF a =
    PutFWord8 !Word8 a
  | PutFInt8 !Int8 a
  | PutFWord16LE !Word16LE a
  | PutFInt16LE !Int16LE a
  | PutFByteString !ByteString a
  | PutFFixedSeq !(PutFixedSeqF a)
  | PutFFixedVector !(PutFixedVectorF a)
  deriving stock (Functor)

newtype PutM a = PutM { unPutM :: F PutF a }
  deriving newtype (Functor, Applicative, Monad)

type Put = PutM ()
