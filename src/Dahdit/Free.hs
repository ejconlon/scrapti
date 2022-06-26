module Dahdit.Free
  ( GetStaticSeqF (..)
  , getStaticSeqSize
  , GetStaticVectorF (..)
  , getStaticVectorSize
  , ScopeMode (..)
  , GetF (..)
  , Get (..)
  , PutStaticSeqF (..)
  , putStaticSeqSize
  , PutStaticVectorF (..)
  , putStaticVectorSize
  , PutF (..)
  , PutM (..)
  , Put
  ) where

import Control.Monad.Free.Church (F (..))
import Dahdit.Nums (Int16LE, Word16LE)
import Dahdit.Proxy (Proxy (..), proxyForF)
import Dahdit.Sizes (ByteCount, ElementCount, StaticByteSized (..))
import Data.ByteString (ByteString)
import Data.Int (Int8)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Foreign.Storable (Storable)

data GetStaticSeqF a where
  GetStaticSeqF :: StaticByteSized z => !ElementCount -> Get z -> (Seq z -> a) -> GetStaticSeqF a

instance Functor GetStaticSeqF where
  fmap f (GetStaticSeqF ec g k) = GetStaticSeqF ec g (f . k)

getStaticSeqSize :: GetStaticSeqF a -> ByteCount
getStaticSeqSize (GetStaticSeqF ec g _) =
  let !z = staticByteSize (proxyForF g)
  in z * fromIntegral ec

data GetStaticVectorF a where
  GetStaticVectorF :: (StaticByteSized z, Storable z) => !ElementCount -> Proxy z -> (VS.Vector z -> a) -> GetStaticVectorF a

instance Functor GetStaticVectorF where
  fmap f (GetStaticVectorF ec prox k) = GetStaticVectorF ec prox (f . k)

getStaticVectorSize :: GetStaticVectorF a -> ByteCount
getStaticVectorSize (GetStaticVectorF ec prox _) =
  let !z = staticByteSize prox
  in z * fromIntegral ec

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
  | GetFStaticSeq !(GetStaticSeqF a)
  | GetFStaticVector !(GetStaticVectorF a)
  | GetFScope !ScopeMode !ByteCount a
  | GetFSkip !ByteCount a
  | GetFFail !String
  deriving stock (Functor)

newtype Get a = Get { unGet :: F GetF a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail Get where
  fail msg = Get (F (\_ y -> y (GetFFail msg)))

data PutStaticSeqF a where
  PutStaticSeqF :: StaticByteSized z => !(Seq z) -> (z -> Put) -> a -> PutStaticSeqF a

instance Functor PutStaticSeqF where
  fmap f (PutStaticSeqF s p k) = PutStaticSeqF s p (f k)

putStaticSeqSize :: PutStaticSeqF a -> ByteCount
putStaticSeqSize (PutStaticSeqF s _ _) =
  let !z = staticByteSize (proxyForF s)
      !ec = fromIntegral (Seq.length s)
  in z * ec

data PutStaticVectorF a where
  PutStaticVectorF :: (StaticByteSized z, Storable z) => !(VS.Vector z) -> a -> PutStaticVectorF a

instance Functor PutStaticVectorF where
  fmap f (PutStaticVectorF n k) = PutStaticVectorF n (f k)

putStaticVectorSize :: PutStaticVectorF a -> ByteCount
putStaticVectorSize (PutStaticVectorF v _) =
  let !z = staticByteSize (proxyForF v)
      !ec = fromIntegral (VS.length v)
  in z * ec

data PutF a =
    PutFWord8 !Word8 a
  | PutFInt8 !Int8 a
  | PutFWord16LE !Word16LE a
  | PutFInt16LE !Int16LE a
  | PutFByteString !ByteString a
  | PutFStaticSeq !(PutStaticSeqF a)
  | PutFStaticVector !(PutStaticVectorF a)
  | PutFStaticHint !ByteCount a
  deriving stock (Functor)

newtype PutM a = PutM { unPutM :: F PutF a }
  deriving newtype (Functor, Applicative, Monad)

type Put = PutM ()
