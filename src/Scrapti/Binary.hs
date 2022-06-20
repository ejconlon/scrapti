module Scrapti.Binary
  ( Get
  , Put
  , ByteLength
  , ByteOffset
  , DecodeState (..)
  , DecodeT (..)
  , runDecodeT
  , DecodeM
  , runDecodeM
  , decode
  , decodeGet
  , guardEnd
  , decodeBounded
  , decodeRepeated
  , runPut
  , getExpect
  , getWord8
  , getWord16le
  , getWord32le
  , getInt8
  , getInt16le
  , getInt32le
  , getInt64le
  , getByteString
  , getVec
  , skip
  , putWord8
  , putWord16le
  , putWord32le
  , putInt8
  , putInt16le
  , putInt32le
  , putInt64le
  , putByteString
  , putVec
  ) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Identity (Identity (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets)
import Control.Monad.Trans (MonadTrans (..))
import Data.Binary.Get (ByteOffset, Get, getByteString, getInt16le, getInt32le, getInt64le, getInt8, getWord16le,
                        getWord32le, getWord8, runGetOrFail, skip)
import Data.Binary.Put (Put, putByteString, putInt16le, putInt32le, putInt64le, putInt8, putWord16le, putWord32le,
                        putWord8, runPut)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.Sequence (Seq (..))
import qualified Data.Vector.Unboxed as VU

type ByteLength = Int64

data DecodeState = DecodeState
  { decStateInput :: !BSL.ByteString
  , decStateOffset :: !ByteOffset
  } deriving stock (Eq, Show)

newtype DecodeT m a = DecodeT { unDecodeT :: ExceptT String (StateT DecodeState m) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DecodeState, MonadIO)

instance Monad m => MonadFail (DecodeT m) where
  fail = DecodeT . throwError

instance MonadTrans DecodeT where
  lift = DecodeT . lift . lift

runDecodeT :: DecodeT m a -> DecodeState -> m (Either String a, DecodeState)
runDecodeT dm = runStateT (runExceptT (unDecodeT dm))

type DecodeM = DecodeT Identity

runDecodeM :: DecodeM a -> DecodeState -> (Either String a, DecodeState)
runDecodeM dm = runIdentity . runDecodeT dm

decode :: MonadFail m => BSL.ByteString -> DecodeT m a -> m a
decode bs act = do
  (ea, _) <- runDecodeT act (DecodeState bs 0)
  either fail pure ea

guardEnd :: Monad m => DecodeT m ()
guardEnd = do
  bs <- gets decStateInput
  unless (BSL.null bs) (fail "not end of input")

decodeBounded :: Monad m => ByteLength -> DecodeT m a -> DecodeT m a
decodeBounded len dec = do
  off <- gets decStateOffset
  let !end = off + len
  elt <- dec
  off' <- gets decStateOffset
  if
    | off' > end -> fail ("consumed too much input: " ++ show (off' - end))
    | off' == end -> pure elt
    | otherwise -> fail ("consumed too little input: " ++ show (end - off'))

decodeRepeated :: Monad m => ByteLength -> DecodeT m a -> DecodeT m (Seq a)
decodeRepeated len dec = do
  off <- gets decStateOffset
  let !end = off + len
  decodeUntil end dec

decodeUntil :: Monad m => ByteOffset -> DecodeT m a -> DecodeT m (Seq a)
decodeUntil end dec = go Empty where
  go !acc = do
    off' <- gets decStateOffset
    if
      | off' > end -> fail ("consumed too much input: " ++ show (off' - end))
      | off' == end -> pure acc
      | otherwise -> do
        elt <- dec
        go (acc :|> elt)

decodeGet :: Monad m => Get a -> DecodeT m a
decodeGet getter = do
  DecodeState bs off <- get
  case runGetOrFail getter bs of
    Left (bs', off', reason) -> do
      put (DecodeState bs' (off + off'))
      fail reason
    Right (bs', off', value) -> do
      put (DecodeState bs' (off + off'))
      pure value

getExpect :: (Eq a, Show a) => String -> Get a -> a -> Get ()
getExpect typ getter expec = do
  actual <- getter
  unless (expec == actual)
    (fail ("Expected " ++ " " ++ typ ++  " " ++ show expec ++ " but found " ++ show actual))

-- getInt24le :: Get Int24
-- getInt24le = get

getVec :: VU.Unbox a => Int -> Get a -> Get (VU.Vector a)
getVec len getter = VU.generateM len (const getter)

-- putInt24le :: Int24 -> Put ()
-- putInt24le = undefined

putVec :: VU.Unbox a => (a -> Put) -> VU.Vector a -> Put
putVec = VU.foldMap'
