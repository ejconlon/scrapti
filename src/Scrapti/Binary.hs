module Scrapti.Binary
  ( Get
  , Put
  , ByteOffset
  , DecodeState (..)
  , DecodeT (..)
  , runDecodeT
  , DecodeM
  , runDecodeM
  , decodeIO
  , decodeGet
  , guardEnd
  , runPut
  , getExpect
  , getWord16le
  , getWord32le
  , getInt8
  , getInt16le
  , getInt32le
  , getInt64le
  , getByteString
  , getVec
  , skip
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
                        getWord32le, runGetOrFail, skip)
import Data.Binary.Put (Put, putByteString, putInt16le, putInt32le, putInt64le, putInt8, putWord16le, putWord32le,
                        runPut)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Unboxed as VU

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

decodeIO :: BSL.ByteString -> DecodeT IO a -> IO a
decodeIO bs act = do
  (ea, _) <- runDecodeT act (DecodeState bs 0)
  either fail pure ea

guardEnd :: Monad m => DecodeT m ()
guardEnd = do
  bs <- gets decStateInput
  unless (BSL.null bs) (fail "not end of input")

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
