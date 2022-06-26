module Scrapti.Parser.Run
  ( runGet
  , runPut
  , runCount
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Free.Church (F (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Control.Monad.State.Strict (MonadState, State, runState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Free (FreeT (..), iterT, wrap)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bits (Bits (..), unsafeShiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BU
import Data.Foldable (for_)
import Data.Int (Int8)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Scrapti.Parser.Free (Get (..), GetF (..), GetStaticSeqF (..), GetStaticVectorF (..), Put, PutF (..), PutM (..),
                            PutStaticSeqF (..), PutStaticVectorF (..))
import Scrapti.Parser.Nums (Int16LE (..), Word16LE (..))
import Scrapti.Parser.Proxy (proxyForF)
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

failNeedBytes :: String -> ByteCount -> ByteCount -> GetEff a
failNeedBytes nm ac bc = fail ("End of input parsing " ++ nm ++ " (have " ++ show (unByteCount ac) ++ " bytes, need " ++ show (unByteCount bc) ++ ")")

readBytes :: String -> ByteCount -> (ByteString -> a) -> GetEff a
readBytes nm bc f = do
  l <- ask
  GetState o bs <- State.get
  let !ac = l - o
  if bc < ac
    then failNeedBytes nm ac bc
    else do
      let a = f bs
          !o' = o + bc
          !bs' = BS.drop (fromIntegral bc) bs
          !st' = GetState o' bs'
      State.put st'
      pure a

readWord8 :: ByteString -> Word8
readWord8 = BU.unsafeHead

readInt8 :: ByteString -> Int8
readInt8 = fromIntegral . readWord8

readWord16LE :: ByteString -> Word16LE
readWord16LE bs = (fromIntegral (bs `BU.unsafeIndex` 1) `unsafeShiftL` 8) .|. fromIntegral (bs `BU.unsafeIndex` 0)

readInt16LE :: ByteString -> Int16LE
readInt16LE = fromIntegral . readWord16LE

execGetRun :: GetF (GetEff a) -> GetEff a
execGetRun = \case
  GetFWord8 k -> readBytes "Word8" 1 readWord8 >>= k
  GetFInt8 k -> readBytes "Int8" 1 readInt8 >>= k
  GetFWord16LE k -> readBytes "Word16LE" 2 readWord16LE >>= k
  GetFInt16LE k -> readBytes "Int16LE" 2 readInt16LE >>= k
  GetFByteString bc k -> readBytes "ByteString" bc (BS.take (fromIntegral bc)) >>= k
  GetFStaticSeq (GetStaticSeqF ec g k) -> do
    error "TODO get seq"
  GetFStaticVector (GetStaticVectorF ec k) -> do
    error "TODO get vector"
  GetFScope sm bc k -> do
    error "TODO get scope"
  GetFSkip bc k -> readBytes "skip" bc (const ()) *> k
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
  PutFStaticSeq (PutStaticSeqF s p k) -> do
    for_ s $ \a -> do
      let !x = p a
      mkPutEff x
    k
  PutFStaticVector (PutStaticVectorF _v _k) -> do
    error "TODO put vector"
  PutFStaticHint _ k -> k

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

newtype CountEff a = CountEff { unCountEff :: MaybeT (State ByteCount) a }
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadState ByteCount)

runCountEff :: CountEff a -> ByteCount -> (Maybe a, ByteCount)
runCountEff m = runState (runMaybeT (unCountEff m))

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
  PutFStaticSeq (PutStaticSeqF s _ k) ->
    let !z = staticByteSize (proxyForF s)
        !ec = fromIntegral (Seq.length s)
        !bc = z * ec
    in State.modify' (bc+) *> k
  PutFStaticVector (PutStaticVectorF v k) ->
    let !z = staticByteSize (proxyForF v)
        !ec = fromIntegral (VS.length v)
        !bc = z * ec
    in State.modify' (bc+) *> k
  PutFStaticHint bc _ -> State.modify' (bc+) *> empty

runCountRun :: CountRun a -> ByteCount -> (Maybe a, ByteCount)
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
      (_, !bs) = runCountRun n 0
  in bs
