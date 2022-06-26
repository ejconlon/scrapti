module Dahdit.Run
  ( GetError (..)
  , prettyGetError
  , runGet
  , PutError (..)
  , prettyPutError
  , runPut
  , runCount
  ) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Free.Church (F (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.State.Strict (MonadState, State, runState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (FreeT (..), iterT, wrap)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Dahdit.Free (Get (..), GetF (..), GetStaticArrayF (..), GetStaticSeqF (..), Put, PutF (..), PutM (..),
                    PutStaticArrayF (..), PutStaticSeqF (..), ScopeMode (..), getStaticArraySize, getStaticSeqSize,
                    putStaticArraySize, putStaticSeqSize)
import Dahdit.Nums (Int16LE (..), Word16LE (..))
import Dahdit.Sizes (ByteCount (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as BSS
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Foldable (for_)
import Data.Int (Int8)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, cloneByteArray, copyByteArray, indexByteArray,
                                 newByteArray, unsafeFreezeByteArray, writeByteArray)
import Data.Primitive.PrimArray (PrimArray (..))
import qualified Data.Sequence as Seq
import Data.STRef.Strict (STRef, modifySTRef', newSTRef, readSTRef)
import Data.Word (Word8)

-- Get:

data GetError =
    GetErrorParseNeed !String !ByteCount !ByteCount
  | GetErrorScopedMismatch !ByteCount !ByteCount
  | GetErrorFail !String
  deriving stock (Eq, Show)

instance Exception GetError where
  displayException = prettyGetError

prettyGetError :: GetError -> String
prettyGetError = \case
  GetErrorParseNeed nm ac bc -> "End of input parsing " ++ nm ++ " (have " ++ show (unByteCount ac) ++ " bytes, need " ++ show (unByteCount bc) ++ ")"
  GetErrorScopedMismatch ac bc -> "Did not parse enough scoped input (read " ++ show (unByteCount ac) ++ " bytes, expected " ++ show (unByteCount bc) ++ ")"
  GetErrorFail msg -> "User error: " ++ msg

data GetEnv s = GetEnv
  { geLen :: !Int
  , gePos :: !(STRef s Int)
  , geArray :: !ByteArray
  }

newGetEnv :: ByteString -> ST s (GetEnv s)
newGetEnv bs = do
  let !sbs@(SBS arr) = BSS.toShort bs
      !len = BSS.length sbs
  pos <- newSTRef 0
  pure $! GetEnv len pos (ByteArray arr)

newtype GetEff s a = GetEff { unGetEff :: ReaderT (GetEnv s) (ExceptT GetError (ST s)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (GetEnv s), MonadError GetError)

runGetEff :: GetEff s a -> GetEnv s -> ST s (Either GetError a)
runGetEff m l = runExceptT (runReaderT (unGetEff m) l)

instance MonadFail (GetEff s) where
  fail = GetEff . throwError . GetErrorFail

stGetEff :: ST s a -> GetEff s a
stGetEff = GetEff . lift . lift

newtype GetRun s a = GetRun { unGetRun :: FreeT GetF (GetEff s) a }
  deriving newtype (Functor, Applicative, Monad)

guardReadBytes :: String -> Int -> GetEff s Int
guardReadBytes nm bc = do
  GetEnv l posRef _ <- ask
  pos <- stGetEff (readSTRef posRef)
  let !ac = l - pos
  if bc > ac
    then throwError (GetErrorParseNeed nm (fromIntegral ac) (fromIntegral bc))
    else pure pos

readBytes :: String -> Int -> (ByteArray -> Int -> a) -> GetEff s a
readBytes nm bc f = do
  pos <- guardReadBytes nm bc
  GetEnv _ posRef arr <- ask
  stGetEff $ do
    let !a = f arr pos
    modifySTRef' posRef (bc+)
    pure a

readByteString :: Int -> ByteArray -> Int -> ShortByteString
readByteString len arr pos = let !(ByteArray frozArr) = cloneByteArray arr pos len in SBS frozArr

readScope :: ScopeMode -> Int -> GetEff s a -> GetEff s a
readScope sm bc g = do
  GetEnv oldLen posRef _ <- ask
  if bc > oldLen
    then throwError (GetErrorParseNeed "scope" (fromIntegral oldLen) (fromIntegral bc))
    else do
      oldPos <- stGetEff (readSTRef posRef)
      let !newLen = oldLen - bc
      a <- local (\ge -> ge { geLen = newLen }) g
      case sm of
        ScopeModeWithin -> pure a
        ScopeModeExact -> do
          newPos <- stGetEff (readSTRef posRef)
          let !actualBc = newPos - oldPos
          if actualBc == bc
            then pure a
            else throwError (GetErrorScopedMismatch (fromIntegral actualBc) (fromIntegral bc))

readStaticSeq :: GetStaticSeqF (GetEff s a) -> GetEff s a
readStaticSeq gss@(GetStaticSeqF ec g k) = do
  let !bc = fromIntegral (getStaticSeqSize gss)
  _ <- guardReadBytes "static sequence" bc
  ss <- Seq.replicateA (fromIntegral ec) (mkGetEff g)
  k ss

readStaticArray :: GetStaticArrayF (GetEff s a) -> GetEff s a
readStaticArray gsa@(GetStaticArrayF _ _ k) = do
  let !bc = fromIntegral (getStaticArraySize gsa)
  sa <- readBytes "static vector" bc (\arr pos -> let !(ByteArray frozArr) = cloneByteArray arr pos bc in PrimArray frozArr)
  k sa

execGetRun :: GetF (GetEff s a) -> GetEff s a
execGetRun = \case
  GetFWord8 k -> readBytes "Word8" 1 (indexByteArray @Word8) >>= k
  GetFInt8 k -> readBytes "Int8" 1 (indexByteArray @Int8) >>= k
  GetFWord16LE k -> readBytes "Word16LE" 2 (indexByteArray @Word16LE) >>= k
  GetFInt16LE k -> readBytes "Int16LE" 2 (indexByteArray @Int16LE) >>= k
  GetFByteString bc k ->
    let !len = fromIntegral bc
    in readBytes "ByteString" len (readByteString len) >>= k
  GetFStaticSeq gss -> readStaticSeq gss
  GetFStaticArray gsa -> readStaticArray gsa
  GetFScope sm bc k -> readScope sm (fromIntegral bc) k
  GetFSkip bc k -> readBytes "skip" (fromIntegral bc) (\_ _ -> ()) *> k
  GetFFail msg -> fail msg

runGetRun :: GetRun s a -> GetEnv s -> ST s (Either GetError a)
runGetRun = runGetEff . iterGetRun

iterGetRun :: GetRun s a -> GetEff s a
iterGetRun m = iterT execGetRun (unGetRun m)

mkGetRun :: Get a -> GetRun s a
mkGetRun (Get (F w)) = GetRun (w pure wrap)

mkGetEff :: Get a -> GetEff s a
mkGetEff = iterGetRun . mkGetRun

runGet :: Get a -> ByteString -> (Either GetError a, ByteCount, ByteArray)
runGet m bs = runST $ do
  let !n = mkGetEff m
  env <- newGetEnv bs
  ea <- runGetEff n env
  bc <- readSTRef (gePos env)
  let !ba = geArray env
  pure (ea, fromIntegral bc, ba)

-- Put:

data PutError = PutError !String !ByteCount !ByteCount
  deriving stock (Eq, Show)

instance Exception PutError where
  displayException = prettyPutError

prettyPutError :: PutError -> String
prettyPutError (PutError nm ac bc) = "End of buffer writing " ++ nm ++ " (have " ++ show (unByteCount ac) ++ ", need " ++ show (unByteCount bc) ++ " more)"

data PutEnv s = PutEnv
  { peLen :: !Int
  , pePos :: !(STRef s Int)
  , peArray :: !(MutableByteArray s)
  }

newPutEnv :: Int -> ST s (PutEnv s)
newPutEnv len = PutEnv len <$> newSTRef 0 <*> newByteArray len

newtype PutEff s a = PutEff { unPutEff :: ReaderT (PutEnv s) (ExceptT PutError (ST s)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (PutEnv s), MonadError PutError)

runPutEff :: PutEff s a -> PutEnv s -> ST s (Either PutError a)
runPutEff m = runExceptT . runReaderT (unPutEff m)

stPutEff :: ST s a -> PutEff s a
stPutEff = PutEff . lift . lift

newtype PutRun s a = PutRun { unPutRun :: FreeT PutF (PutEff s) a }
  deriving newtype (Functor, Applicative, Monad)

guardWriteBytes :: String -> Int -> PutEff s Int
guardWriteBytes nm bc = do
  PutEnv l posRef _ <- ask
  pos <- stPutEff (readSTRef posRef)
  let !ac = l - pos
  if bc > ac
    then throwError (PutError nm (fromIntegral ac) (fromIntegral bc))
    else pure pos

writeBytes :: String -> Int -> (MutableByteArray s -> Int -> ST s ()) -> PutEff s ()
writeBytes nm bc f = do
  pos <- guardWriteBytes nm bc
  PutEnv _ posRef arr <- ask
  stPutEff $ do
    f arr pos
    modifySTRef' posRef (bc+)

writeByteString :: ShortByteString -> MutableByteArray s -> Int -> ST s ()
writeByteString sbs@(SBS frozArr) arr pos =
  let !len = BSS.length sbs
  in copyByteArray arr pos (ByteArray frozArr) 0 len

writeStaticSeq :: PutStaticSeqF (PutEff s a) -> PutEff s a
writeStaticSeq pss@(PutStaticSeqF ss p k) = do
  let !bc = fromIntegral (putStaticSeqSize pss)
  _ <- guardWriteBytes "static sequence" bc
  for_ ss $ \a -> do
    let !x = p a
    mkPutEff x
  k

writeStaticArray :: PutStaticArrayF (PutEff s a) -> PutEff s a
writeStaticArray psa@(PutStaticArrayF sv k) = do
  let !bc = fromIntegral (putStaticArraySize psa)
      !(PrimArray frozArr) = sv
  writeBytes "static vector" bc (\arr pos -> copyByteArray arr pos (ByteArray frozArr) 0 bc)
  k

execPutRun :: PutF (PutEff s a) -> PutEff s a
execPutRun = \case
  PutFWord8 x k -> writeBytes "Word8" 1 (\arr pos -> writeByteArray arr pos x) *> k
  PutFInt8 x k -> writeBytes "Int8" 1 (\arr pos -> writeByteArray arr pos x) *> k
  PutFWord16LE x k -> writeBytes "Word16LE" 2 (\arr pos -> writeByteArray arr pos x) *> k
  PutFInt16LE x k -> writeBytes "Int16LE" 2 (\arr pos -> writeByteArray arr pos x) *> k
  PutFByteString sbs k -> writeBytes "ByteString" (BSS.length sbs) (writeByteString sbs) *> k
  PutFStaticSeq pss -> writeStaticSeq pss
  PutFStaticArray psa -> writeStaticArray psa
  PutFStaticHint _ k -> k

runPutRun :: PutRun s a -> PutEnv s -> ST s (Either PutError a)
runPutRun = runPutEff . iterPutRun

iterPutRun :: PutRun s a -> PutEff s a
iterPutRun m = iterT execPutRun (unPutRun m)

mkPutRun :: PutM a -> PutRun s a
mkPutRun (PutM (F w)) = PutRun (w pure wrap)

mkPutEff :: PutM a -> PutEff s a
mkPutEff = iterPutRun . mkPutRun

runPut :: Put -> ByteCount -> Either PutError (ByteCount, ByteArray)
runPut m bc = runST $ do
  let !len = fromIntegral bc
      !n = mkPutRun m
  st@(PutEnv _ posRef arr) <- newPutEnv len
  ea <- runPutRun n st
  case ea of
    Left e -> pure (Left e)
    Right () -> do
      pos <- fmap fromIntegral (readSTRef posRef)
      frozArr <- unsafeFreezeByteArray arr
      pure (Right (pos, frozArr))

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
    let !bc = fromIntegral (BSS.length bs)
    in State.modify' (bc+) *> k
  PutFStaticSeq pss@(PutStaticSeqF _ _ k) ->
    let !bc = putStaticSeqSize pss
    in State.modify' (bc+) *> k
  PutFStaticArray psv@(PutStaticArrayF _ k) ->
    let !bc = putStaticArraySize psv
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
