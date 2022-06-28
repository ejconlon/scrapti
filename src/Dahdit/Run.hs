module Dahdit.Run
  ( GetError (..)
  , prettyGetError
  , runGet
  , runGetIO
  , runCount
  , runPut
  ) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..), throwIO)
import Control.Monad (replicateM_, unless)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Free.Church (F (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.State.Strict (MonadState, State, runState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (FreeT (..), iterT, wrap)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Dahdit.Free (Get (..), GetF (..), GetLookAheadF (..), GetScopeF (..), GetStaticArrayF (..), GetStaticSeqF (..),
                    Put, PutF (..), PutM (..), PutStaticArrayF (..), PutStaticHintF (..), PutStaticSeqF (..),
                    ScopeMode (..))
import Dahdit.Nums (FloatLE, Int16LE (..), Int32LE, LiftedPrim (..), Word16LE (..), Word32LE)
import Dahdit.Proxy (proxyForF)
import Dahdit.Sizes (ByteCount (..), staticByteSize)
import qualified Data.ByteString.Short as BSS
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Foldable (for_, toList)
import Data.Int (Int8)
import Data.Maybe (fromJust)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, cloneByteArray, copyByteArray, indexByteArray,
                                 newByteArray, setByteArray, unsafeFreezeByteArray, writeByteArray)
import Data.Primitive.PrimArray (PrimArray (..), sizeofPrimArray)
import qualified Data.Sequence as Seq
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word8)

-- Sizes:

getStaticSeqSize :: GetStaticSeqF a -> Int
getStaticSeqSize (GetStaticSeqF ec g _) =
  let !z = fromIntegral (staticByteSize (proxyForF g))
  in z * fromIntegral ec

getStaticArraySize :: GetStaticArrayF a -> Int
getStaticArraySize (GetStaticArrayF n prox _) =
  let !z = fromIntegral (staticByteSize prox)
  in z * fromIntegral n

putStaticSeqSize :: PutStaticSeqF a -> Int
putStaticSeqSize (PutStaticSeqF n _ _ s _) =
  let !z = fromIntegral (staticByteSize (proxyForF s))
  in z * fromIntegral n

putStaticArrayElemSize :: PutStaticArrayF a -> Int
putStaticArrayElemSize (PutStaticArrayF _ _ a _) =
  fromIntegral (staticByteSize (proxyForF a))

putStaticArraySize :: PutStaticArrayF a -> Int
putStaticArraySize (PutStaticArrayF n _ a _) =
  let !z = fromIntegral (staticByteSize (proxyForF a))
  in z * fromIntegral n

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

newGetEnv :: ShortByteString -> ST s (GetEnv s)
newGetEnv sbs@(SBS arr) = do
  let !len = BSS.length sbs
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
        !newPos = pos + bc
    writeSTRef posRef newPos
    pure a

readShortByteString :: Int -> ByteArray -> Int -> ShortByteString
readShortByteString len arr pos = let !(ByteArray frozArr) = cloneByteArray arr pos len in SBS frozArr

readScope :: GetScopeF (GetEff s a) -> GetEff s a
readScope (GetScopeF sm bc g k) = do
  let intBc = fromIntegral bc
  GetEnv oldLen posRef _ <- ask
  oldPos <- stGetEff (readSTRef posRef)
  let !oldAvail = oldLen - oldPos
  if intBc > oldAvail
    then throwError (GetErrorParseNeed "scope" (fromIntegral oldAvail) bc)
    else do
      let !newLen = oldPos + intBc
      a <- local (\ge -> ge { geLen = newLen }) (mkGetEff g)
      case sm of
        ScopeModeWithin -> k a
        ScopeModeExact -> do
          newPos <- stGetEff (readSTRef posRef)
          let !actualBc = newPos - oldPos
          if actualBc == intBc
            then k a
            else throwError (GetErrorScopedMismatch (fromIntegral actualBc) bc)

readStaticSeq :: GetStaticSeqF (GetEff s a) -> GetEff s a
readStaticSeq gss@(GetStaticSeqF ec g k) = do
  let !bc = getStaticSeqSize gss
  _ <- guardReadBytes "static sequence" bc
  ss <- Seq.replicateA (fromIntegral ec) (mkGetEff g)
  k ss

readStaticArray :: GetStaticArrayF (GetEff s a) -> GetEff s a
readStaticArray gsa@(GetStaticArrayF _ _ k) = do
  let !bc = getStaticArraySize gsa
  sa <- readBytes "static vector" bc (\arr pos -> let !(ByteArray frozArr) = cloneByteArray arr pos bc in PrimArray frozArr)
  k sa

readLookAhead :: GetLookAheadF (GetEff s a) -> GetEff s a
readLookAhead (GetLookAheadF g k) = do
  posRef <- asks gePos
  startPos <- stGetEff (readSTRef posRef)
  a <- mkGetEff g
  stGetEff (writeSTRef posRef startPos)
  k a

execGetRun :: GetF (GetEff s a) -> GetEff s a
execGetRun = \case
  GetFWord8 k -> readBytes "Word8" 1 (indexByteArray @Word8) >>= k
  GetFInt8 k -> readBytes "Int8" 1 (indexByteArray @Int8) >>= k
  GetFWord16LE k -> readBytes "Word16LE" 2 (indexByteArrayLifted @Word16LE) >>= k
  GetFInt16LE k -> readBytes "Int16LE" 2 (indexByteArrayLifted @Int16LE) >>= k
  GetFWord32LE k -> readBytes "Word32LE" 4 (indexByteArrayLifted @Word32LE) >>= k
  GetFInt32LE k -> readBytes "Int32LE" 4 (indexByteArrayLifted @Int32LE) >>= k
  GetFFloatLE k -> readBytes "FloatLE" 4 (indexByteArrayLifted @FloatLE) >>= k
  GetFShortByteString bc k ->
    let !len = fromIntegral bc
    in readBytes "ShortByteString" len (readShortByteString len) >>= k
  GetFStaticSeq gss -> readStaticSeq gss
  GetFStaticArray gsa -> readStaticArray gsa
  GetFScope gs -> readScope gs
  GetFSkip bc k -> readBytes "skip" (fromIntegral bc) (\_ _ -> ()) *> k
  GetFLookAhead gla -> readLookAhead gla
  GetFRemainingSize k -> do
    GetEnv len posRef _ <- ask
    pos <- stGetEff (readSTRef posRef)
    let !bc = fromIntegral (len - pos)
    k bc
  GetFFail msg -> fail msg

runGetRun :: GetRun s a -> GetEnv s -> ST s (Either GetError a)
runGetRun = runGetEff . iterGetRun

iterGetRun :: GetRun s a -> GetEff s a
iterGetRun m = iterT execGetRun (unGetRun m)

mkGetRun :: Get a -> GetRun s a
mkGetRun (Get (F w)) = GetRun (w pure wrap)

mkGetEff :: Get a -> GetEff s a
mkGetEff = iterGetRun . mkGetRun

runGet :: Get a -> ShortByteString -> (Either GetError a, ByteCount)
runGet m bs = runST $ do
  let !n = mkGetEff m
  env <- newGetEnv bs
  ea <- runGetEff n env
  bc <- readSTRef (gePos env)
  pure (ea, fromIntegral bc)

runGetIO :: Get a -> ShortByteString -> IO (a, ByteCount)
runGetIO m bs =
  let (!ea, !bc) = runGet m bs
  in case ea of
    Left e -> throwIO e
    Right a -> pure (a, bc)

-- Put unsafe:

data PutEnv s = PutEnv
  { peLen :: !Int
  , pePos :: !(STRef s Int)
  , peArray :: !(MutableByteArray s)
  }

newPutEnv :: Int -> ST s (PutEnv s)
newPutEnv len = PutEnv len <$> newSTRef 0 <*> newByteArray len

newtype PutEff s a = PutEff { unPutEff :: ReaderT (PutEnv s) (ST s) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (PutEnv s))

runPutEff :: PutEff s a -> PutEnv s -> ST s a
runPutEff m = runReaderT (unPutEff m)

stPutEff :: ST s a -> PutEff s a
stPutEff = PutEff . lift

newtype PutRun s a = PutRun { unPutRun :: FreeT PutF (PutEff s) a }
  deriving newtype (Functor, Applicative, Monad)

writeBytes :: Int -> (MutableByteArray s -> Int -> ST s ()) -> PutEff s ()
writeBytes bc f = do
  PutEnv _ posRef arr <- ask
  stPutEff $ do
    pos <- readSTRef posRef
    f arr pos
    let !newPos = pos + bc
    writeSTRef posRef newPos

writeShortByteString :: ShortByteString -> Int -> MutableByteArray s -> Int -> ST s ()
writeShortByteString (SBS frozArr) len arr pos = copyByteArray arr pos (ByteArray frozArr) 0 len

writeStaticSeq :: PutStaticSeqF (PutEff s a) -> PutEff s a
writeStaticSeq (PutStaticSeqF n mz p s k) = do
  let n' = fromIntegral n
  for_ (take n' (toList s)) $ \a -> do
    let !x = p a
    mkPutEff x
  let !e = Seq.length s
  unless (n' <= e) $ do
    let !q = mkPutEff (p (fromJust mz))
    replicateM_ (n' - e) q
  k

writeStaticArray :: PutStaticArrayF (PutEff s a) -> PutEff s a
writeStaticArray psa@(PutStaticArrayF needElems mz a@(PrimArray frozArr) k) = do
  let !elemSize = putStaticArrayElemSize psa
      !haveElems = sizeofPrimArray a
      !useElems = min haveElems (fromIntegral needElems)
      !useBc = elemSize * useElems
  writeBytes useBc (\arr pos -> copyByteArray arr pos (ByteArray frozArr) 0 useBc)
  let !needBc = putStaticArraySize psa
  unless (useBc == needBc) $ do
    let !extraBc = needBc - useBc
    writeBytes extraBc (\arr pos -> setByteArray arr (pos + useBc) (pos + extraBc) (fromJust mz))
  k

execPutRun :: PutF (PutEff s a) -> PutEff s a
execPutRun = \case
  PutFWord8 x k -> writeBytes 1 (\arr pos -> writeByteArray arr pos x) *> k
  PutFInt8 x k -> writeBytes 1 (\arr pos -> writeByteArray arr pos x) *> k
  PutFWord16LE x k -> writeBytes 2 (writeByteArrayLifted x) *> k
  PutFInt16LE x k -> writeBytes 2 (writeByteArrayLifted x) *> k
  PutFWord32LE x k -> writeBytes 4 (writeByteArrayLifted x) *> k
  PutFInt32LE x k -> writeBytes 4 (writeByteArrayLifted x) *> k
  PutFFloatLE x k -> writeBytes 4 (writeByteArrayLifted x) *> k
  PutFShortByteString bc sbs k ->
    let !len = fromIntegral bc
    in writeBytes len (writeShortByteString sbs len) *> k
  PutFStaticSeq pss -> writeStaticSeq pss
  PutFStaticArray psa -> writeStaticArray psa
  PutFStaticHint (PutStaticHintF _ p k) -> mkPutEff p *> k

runPutRun :: PutRun s a -> PutEnv s -> ST s a
runPutRun = runPutEff . iterPutRun

iterPutRun :: PutRun s a -> PutEff s a
iterPutRun m = iterT execPutRun (unPutRun m)

mkPutRun :: PutM a -> PutRun s a
mkPutRun (PutM (F w)) = PutRun (w pure wrap)

mkPutEff :: PutM a -> PutEff s a
mkPutEff = iterPutRun . mkPutRun

runPutUnsafe :: Put -> ByteCount -> ShortByteString
runPutUnsafe m bc = runST $ do
  let !len = fromIntegral bc
      !n = mkPutRun m
  st@(PutEnv _ posRef arr) <- newPutEnv len
  runPutRun n st
  pos <- readSTRef posRef
  unless (pos == len) (error ("Invalid put length: (given " ++ show len ++ ", used " ++ show pos ++ ")"))
  ByteArray frozArr <- unsafeFreezeByteArray arr
  pure $! SBS frozArr

-- Count:

newtype CountEff a = CountEff { unCountEff :: MaybeT (State Int) a }
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadState Int)

runCountEff :: CountEff a -> Int -> (Maybe a, Int)
runCountEff m = runState (runMaybeT (unCountEff m))

newtype CountRun a = CountRun { unCountRun :: FreeT PutF CountEff a }
  deriving newtype (Functor, Applicative, Monad)

execCountRun :: PutF (CountEff a) -> CountEff a
execCountRun = \case
  PutFWord8 _ k -> State.modify' (1+) *> k
  PutFInt8 _ k -> State.modify' (1+) *> k
  PutFWord16LE _ k -> State.modify' (2+) *> k
  PutFInt16LE _ k -> State.modify' (2+) *> k
  PutFWord32LE _ k -> State.modify' (4+) *> k
  PutFInt32LE _ k -> State.modify' (4+) *> k
  PutFFloatLE _ k -> State.modify' (4+) *> k
  PutFShortByteString bc _ k ->
    let !len = fromIntegral bc
    in State.modify' (len+) *> k
  PutFStaticSeq pss@(PutStaticSeqF _ _ _ _ k) ->
    let !len = putStaticSeqSize pss
    in State.modify' (len+) *> k
  PutFStaticArray psv@(PutStaticArrayF _ _ _ k) ->
    let !len = putStaticArraySize psv
    in State.modify' (len+) *> k
  PutFStaticHint (PutStaticHintF bc _ k) ->
    let !len = fromIntegral bc
    in State.modify' (len+) *> k

runCountRun :: CountRun a -> Int -> (Maybe a, Int)
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
      (_, !bc) = runCountRun n 0
  in fromIntegral bc

-- Put safe:

runPut :: Put -> ShortByteString
runPut m = let !bc = runCount m in runPutUnsafe m bc
