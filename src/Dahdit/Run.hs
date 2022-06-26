module Dahdit.Run
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
import Dahdit.Free (Get (..), GetF (..), GetStaticSeqF (..), GetStaticVectorF (..), Put, PutF (..), PutM (..),
                    PutStaticSeqF (..), PutStaticVectorF (..), getStaticSeqSize, getStaticVectorSize, putStaticSeqSize,
                    putStaticVectorSize, ScopeMode)
import Dahdit.Nums (Int16LE (..), Word16LE (..))
import Dahdit.Proxy (proxyForF, Proxy (..))
import Dahdit.Sizes (ByteCount (..), ElementCount (..), staticByteSize)
import Data.Bits (Bits (..), unsafeShiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BU
import Data.Foldable (for_)
import Data.Int (Int8)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Debug.Trace (traceM)
import Foreign.Storable (Storable)
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (castForeignPtr, plusForeignPtr)

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

failNeedBytesM :: String -> ByteCount -> ByteCount -> GetEff a
failNeedBytesM nm ac bc = fail ("End of input parsing " ++ nm ++ " (have " ++ show (unByteCount ac) ++ " bytes, need " ++ show (unByteCount bc) ++ ")")

readBytesM :: String -> ByteCount -> (ByteString -> a) -> GetEff a
readBytesM nm bc f = do
  l <- ask
  GetState o bs <- State.get
  let !ac = l - o
  if bc > ac
    then failNeedBytesM nm ac bc
    else do
      let !a = f bs
          !o' = o + bc
          !bs' = BS.drop (fromIntegral bc) bs
          !st' = GetState o' bs'
      State.put st'
      pure a

readScopeM :: ScopeMode -> ByteCount -> GetEff a -> GetEff a
readScopeM sm bc g = do
  -- TODO RECORD IT
  a <- g
  -- TODO CHECK IT
  pure a

readWord8 :: ByteString -> Word8
readWord8 = BU.unsafeHead

readInt8 :: ByteString -> Int8
readInt8 = fromIntegral . readWord8

readWord16LE :: ByteString -> Word16LE
readWord16LE bs = (fromIntegral (bs `BU.unsafeIndex` 1) `unsafeShiftL` 8) .|. fromIntegral (bs `BU.unsafeIndex` 0)

readInt16LE :: ByteString -> Int16LE
readInt16LE = fromIntegral . readWord16LE

readStaticSeqM :: GetStaticSeqF (GetEff a) -> GetEff a
readStaticSeqM = undefined
-- let !bc = getStaticSeqSize gss
-- in readBytesM "static sequence" bc (readStaticSeq ec g) >>= k
-- readStaticSeqM ec g = go Empty 0 where
--   n = mkGetRun g
--   go !acc !i !bs = do
--     if i == ec
--       then Right acc
--       else do
--         let !l = fromIntegral (BS.length bs)
--             !s = newGetState bs
--             !(ea, GetState _ bs') = runGetRun n l s
--         a <- ea
--         go (acc :|> a) (i + 1) bs'

-- NOTE This is all wrong BC of pinning - just use ShortByteString everywhere
-- And use Data.Primitive.ByteArray instead of vector
readStaticVector :: Storable a => Proxy a -> ByteCount -> ByteCount -> ByteString -> VS.Vector a
readStaticVector _ pbc bc fullBs = vec where
  partBs = BS.take (fromIntegral bc) fullBs
  -- Vec pinning from https://github.com/sheyll/bytestring-to-vector/pull/1
  vec = VS.unsafeFromForeignPtr (castForeignPtr (fptr `plusForeignPtr` off)) 0 (scale len)
  (fptr, off, len) = BSI.toForeignPtr partBs
  scale = (`div` fromIntegral pbc)

execGetRun :: GetF (GetEff a) -> GetEff a
execGetRun = \case
  GetFWord8 k -> readBytesM "Word8" 1 readWord8 >>= k
  GetFInt8 k -> readBytesM "Int8" 1 readInt8 >>= k
  GetFWord16LE k -> readBytesM "Word16LE" 2 readWord16LE >>= k
  GetFInt16LE k -> readBytesM "Int16LE" 2 readInt16LE >>= k
  GetFByteString bc k -> readBytesM "ByteString" bc (BS.take (fromIntegral bc)) >>= k
  GetFStaticSeq gss -> readStaticSeqM gss
  GetFStaticVector gsv@(GetStaticVectorF _ p k) -> do
    let !pbc = staticByteSize p
        !bc = getStaticVectorSize gsv
    readBytesM "static vector" bc (readStaticVector p pbc bc) >>= k
  GetFScope sm bc k -> readScopeM sm bc k
  GetFSkip bc k -> readBytesM "skip" bc (const ()) *> k
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
  let !n = mkGetRun m
      !l = fromIntegral (BS.length bs)
      !s = newGetState bs
      !(ea, GetState o bs') = runGetRun n l s
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
  let !n = mkPutRun m
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
  PutFStaticSeq pss@(PutStaticSeqF _ _ k) ->
    let !bc = putStaticSeqSize pss
    in State.modify' (bc+) *> k
  PutFStaticVector psv@(PutStaticVectorF _ k) ->
    let !bc = putStaticVectorSize psv
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
  let !n = mkCountRun m
      (_, !bs) = runCountRun n 0
  in bs
