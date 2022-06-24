module Scrapti.Parser.Run
  ( runGet
  , runPut
  , runCount
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Free.Church (F (..))
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.State.Strict (MonadState, State, runState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Free (FreeT (..), iterT, wrap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Primitive as VP
import Scrapti.Parser.Free (Get (..), GetF (..), GetFixedSeqF (..), GetFixedVectorF (..), Put, PutF (..),
                            PutFixedSeqF (..), PutFixedVectorF (..), PutM (..))
import Scrapti.Parser.Nums (Int16LE (..), Word16LE (..))
import Scrapti.Parser.Sizes (ByteCount (..), staticByteSize)

data GetState = GetState
  { gsOffset :: !ByteCount
  , gsContents :: !ByteString
  } deriving stock (Eq, Show)

newGetState :: ByteString -> GetState
newGetState = GetState 0

newtype GetEff a = GetEff { unGetEff :: ReaderT ByteCount (ExceptT String (State GetState)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader ByteCount, MonadState GetState)

runGetEff :: GetEff a -> ByteCount -> GetState -> (Either String a, GetState)
runGetEff m l = runState (runExceptT (runReaderT (unGetEff m) l))

instance MonadFail GetEff where
  fail = GetEff . throwError

newtype GetRun a = GetRun { unGetRun :: FreeT GetF GetEff a }
  deriving newtype (Functor, Applicative, Monad)

failNeedBytes :: String -> ByteCount -> GetEff a
failNeedBytes tyName numBytes = fail ("End of input parsing " ++ tyName ++ " (" ++ show numBytes ++ " bytes)")

execGetRun :: GetF (GetEff a) -> GetEff a
execGetRun = \case
  GetFWord8 k -> do
    GetState o bs <- State.get
    case BS.uncons bs of
      Nothing -> failNeedBytes "Word8" 1
      Just _ -> undefined
  GetFInt8 k -> undefined
  GetFWord16LE k -> undefined
  GetFInt16LE k -> undefined
  GetFByteString bc k -> undefined
  GetFFixedSeq (GetFixedSeqF ec g k) -> undefined
  GetFFixedVector (GetFixedVectorF ec g k) -> undefined
  GetFScope sm bc k -> undefined
  GetFSkip bc k -> undefined
  GetFFail msg -> fail msg

runGetRun :: GetRun a -> ByteCount -> GetState -> (Either String a, GetState)
runGetRun = runGetEff . iterGetRun

iterGetRun :: GetRun a -> GetEff a
iterGetRun m = iterT execGetRun (unGetRun m)

mkGetRun :: Get a -> GetRun a
mkGetRun (Get (F w)) = GetRun (w pure wrap)

mkGetEff :: Get a -> GetEff a
mkGetEff = iterGetRun . mkGetRun

runGet :: Get a -> ByteString -> (Either String a, ByteCount, ByteString)
runGet m bs =
  let n = mkGetRun m
      l = fromIntegral (BS.length bs)
      s = newGetState bs
      (ea, GetState o bs') = runGetRun n l s
  in (ea, o, bs')

newtype PutEff a = PutEff { unPutEff :: State Builder a }
  deriving newtype (Functor, Applicative, Monad, MonadState Builder)

runPutEff :: PutEff a -> Builder -> (a, Builder)
runPutEff m = runState (unPutEff m)

newtype PutRun a = PutRun { unPutRun :: FreeT PutF PutEff a }
  deriving newtype (Functor, Applicative, Monad)

execPutRun :: PutF (PutEff a) -> PutEff a
execPutRun = \case
  PutFWord8 x k -> State.modify' (<> BSB.word8 x) *> k
  PutFInt8 x k -> State.modify' (<> BSB.int8 x) *> k
  PutFWord16LE x k -> State.modify' (<> BSB.word16LE (unWord16LE x)) *> k
  PutFInt16LE x k -> State.modify' (<> BSB.int16LE (unInt16LE x)) *> k
  PutFByteString bs k -> State.modify' (<> BSB.byteString bs) *> k
  PutFFixedSeq (PutFixedSeqF s p k) -> do
    for_ s $ \a -> do
      let !x = p a
      mkPutEff x
    k
  PutFFixedVector (PutFixedVectorF v p k) -> do
    VP.forM_ v $ \a -> do
      let !x = p a
      mkPutEff x
    k

runPutRun :: PutRun a -> Builder -> (a, Builder)
runPutRun = runPutEff . iterPutRun

iterPutRun :: PutRun a -> PutEff a
iterPutRun m = iterT execPutRun (unPutRun m)

mkPutRun :: PutM a -> PutRun a
mkPutRun (PutM (F w)) = PutRun (w pure wrap)

mkPutEff :: PutM a -> PutEff a
mkPutEff = iterPutRun . mkPutRun

runPut :: Put -> ByteString
runPut m =
  let n = mkPutRun m
      ((), !b) = runPutRun n mempty
  in BSL.toStrict (BSB.toLazyByteString b)

-- Count:

newtype CountEff a = CountEff { unCountEff :: State ByteCount a }
  deriving newtype (Functor, Applicative, Monad, MonadState ByteCount)

runCountEff :: CountEff a -> ByteCount -> (a, ByteCount)
runCountEff m = runState (unCountEff m)

newtype CountRun a = CountRun { unCountRun :: FreeT PutF CountEff a }
  deriving newtype (Functor, Applicative, Monad)

execCountRun :: PutF (CountEff a) -> CountEff a
execCountRun = \case
  PutFWord8 _ k -> State.modify' (1+) *> k
  PutFInt8 _ k -> State.modify' (1+) *> k
  PutFWord16LE _ k -> State.modify' (2+) *> k
  PutFInt16LE _ k -> State.modify' (2+) *> k
  PutFByteString bs k ->
    let !bc = fromIntegral (BS.length bs)
    in State.modify' (bc+) *> k
  PutFFixedSeq (PutFixedSeqF s _ k) ->
    let !z = staticByteSize s
        !ec = fromIntegral (Seq.length s)
        !bc = z * ec
    in State.modify' (bc+) *> k
  PutFFixedVector (PutFixedVectorF v _ k) ->
    let !z = staticByteSize v
        !ec = fromIntegral (VP.length v)
        !bc = z * ec
    in State.modify' (bc+) *> k

runCountRun :: CountRun a -> ByteCount -> (a, ByteCount)
runCountRun = runCountEff . iterCountRun

iterCountRun :: CountRun a -> CountEff a
iterCountRun m = iterT execCountRun (unCountRun m)

mkCountRun :: PutM a -> CountRun a
mkCountRun (PutM (F w)) = CountRun (w pure wrap)

mkCountEff :: PutM a -> CountEff a
mkCountEff = iterCountRun . mkCountRun

runCount :: Put -> ByteCount
runCount m =
  let n = mkCountRun m
      ((), !bs) = runCountRun n 0
  in bs
