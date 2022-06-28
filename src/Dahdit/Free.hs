module Dahdit.Free
  ( GetStaticSeqF (..)
  , GetStaticArrayF (..)
  , GetLookAheadF (..)
  , GetScopeF (..)
  , ScopeMode (..)
  , GetF (..)
  , Get (..)
  , PutStaticSeqF (..)
  , PutStaticArrayF (..)
  , PutStaticHintF (..)
  , PutF (..)
  , PutM (..)
  , Put
  ) where

import Control.Monad.Free.Church (F (..))
import Dahdit.Nums (FloatLE, Int16LE, Int32LE, Word16LE, Word32LE)
import Dahdit.Proxy (Proxy (..))
import Dahdit.Sizes (ByteCount, ElementCount, StaticByteSized (..))
import Data.ByteString.Short (ShortByteString)
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Primitive.PrimArray (PrimArray)
import Data.Sequence (Seq)
import Data.Word (Word8)

data GetStaticSeqF a where
  GetStaticSeqF :: (StaticByteSized z) => !ElementCount -> Get z -> (Seq z -> a) -> GetStaticSeqF a

instance Functor GetStaticSeqF where
  fmap f (GetStaticSeqF n g k) = GetStaticSeqF n g (f . k)

data GetStaticArrayF a where
  GetStaticArrayF :: (StaticByteSized z, Prim z) => !ElementCount -> Proxy z -> (PrimArray z -> a) -> GetStaticArrayF a

instance Functor GetStaticArrayF where
  fmap f (GetStaticArrayF n p k) = GetStaticArrayF n p (f . k)

data GetLookAheadF a where
  GetLookAheadF :: Get z -> (z -> a) -> GetLookAheadF a

instance Functor GetLookAheadF where
  fmap f (GetLookAheadF g k) = GetLookAheadF g (f . k)

data GetScopeF a where
  GetScopeF :: !ScopeMode -> !ByteCount -> Get z -> (z -> a) -> GetScopeF a

instance Functor GetScopeF where
  fmap f (GetScopeF sm bc g k) = GetScopeF sm bc g (f . k)

data ScopeMode =
    ScopeModeExact
  | ScopeModeWithin
  deriving stock (Eq, Show)

data GetF a =
    GetFWord8 (Word8 -> a)
  | GetFInt8 (Int8 -> a)
  | GetFWord16LE (Word16LE -> a)
  | GetFInt16LE (Int16LE -> a)
  | GetFWord32LE (Word32LE -> a)
  | GetFInt32LE (Int32LE -> a)
  | GetFFloatLE (FloatLE -> a)
  | GetFShortByteString !ByteCount (ShortByteString -> a)
  | GetFStaticSeq !(GetStaticSeqF a)
  | GetFStaticArray !(GetStaticArrayF a)
  | GetFScope !(GetScopeF a)
  | GetFSkip !ByteCount a
  | GetFLookAhead !(GetLookAheadF a)
  | GetFRemainingSize (ByteCount -> a)
  | GetFFail !String
  deriving stock (Functor)

newtype Get a = Get { unGet :: F GetF a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail Get where
  fail msg = Get (F (\_ y -> y (GetFFail msg)))

data PutStaticSeqF a where
  PutStaticSeqF :: StaticByteSized z => !ElementCount -> !(Maybe z) -> (z -> Put) -> !(Seq z) -> a -> PutStaticSeqF a

instance Functor PutStaticSeqF where
  fmap f (PutStaticSeqF n z p s k) = PutStaticSeqF n z p s (f k)

data PutStaticArrayF a where
  PutStaticArrayF :: (StaticByteSized z, Prim z) => !ElementCount -> !(Maybe z) -> !(PrimArray z) -> a -> PutStaticArrayF a

instance Functor PutStaticArrayF where
  fmap f (PutStaticArrayF n z a k) = PutStaticArrayF n z a (f k)

data PutStaticHintF a where
  PutStaticHintF :: !ByteCount -> Put -> a -> PutStaticHintF a

instance Functor PutStaticHintF where
  fmap f (PutStaticHintF n p k) = PutStaticHintF n p (f k)

data PutF a =
    PutFWord8 !Word8 a
  | PutFInt8 !Int8 a
  | PutFWord16LE !Word16LE a
  | PutFInt16LE !Int16LE a
  | PutFWord32LE !Word32LE a
  | PutFInt32LE !Int32LE a
  | PutFFloatLE !FloatLE a
  | PutFShortByteString !ByteCount !ShortByteString a
  | PutFStaticSeq !(PutStaticSeqF a)
  | PutFStaticArray !(PutStaticArrayF a)
  | PutFStaticHint !(PutStaticHintF a)
  deriving stock (Functor)

newtype PutM a = PutM { unPutM :: F PutF a }
  deriving newtype (Functor, Applicative, Monad)

instance Semigroup (PutM ()) where
  p <> q = p *> q

instance Monoid (PutM ()) where
  mappend = (<>)
  mempty = pure ()

type Put = PutM ()
