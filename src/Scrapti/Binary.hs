{-# LANGUAGE UnboxedTuples #-}

module Scrapti.Binary
  ( Binary (..)
  , Get
  , Put
  , runPut
  , ByteLength (..)
  , ByteOffset
  , DecodeState (..)
  , DecodeT (..)
  , runDecodeT
  , DecodeM
  , runDecodeM
  , decodeFail
  , decodeGet
  , guardEnd
  , decodeBounded
  , decodeFolded
  , decodeRepeated
  , SizedBinary (..)
  , BoolByte (..)
  , FloatLE (..)
  , Word16LE (..)
  , Int16LE (..)
  , Word32LE (..)
  , Int32LE (..)
  , Word64LE (..)
  , Int64LE (..)
  , TermText (..)
  , FixedText (..)
  , FixedBytes (..)
  , getByteString
  , getExpect
  , getVec
  , getVecWith
  , getSeq
  , getSeqWith
  , skip
  , putByteString
  , putVec
  , putVecWith
  , putSeq
  , putSeqWith
  ) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Identity (Identity (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState, StateT (..), gets)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (MonadTrans (..))
import Data.Binary (Binary (..))
import Data.Binary.Get (ByteOffset, Get, getByteString, getFloatle, getInt16le, getInt32le, getInt64le, getWord16le,
                        getWord32le, getWord64le, getWord8, runGetOrFail, skip)
import Data.Binary.Put (Put, putByteString, putFloatle, putInt16le, putInt32le, putInt64le, putWord16le, putWord32le,
                        putWord64le, putWord8, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Default (Default (..))
import Data.Foldable (toList, traverse_)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive (Prim)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Primitive as VP
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, Nat, natVal)

newtype ByteLength = ByteLength { unByteLength :: Int64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default)

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

decodeFail :: MonadFail m => BSL.ByteString -> DecodeT m a -> m a
decodeFail bs act = do
  (ea, _) <- runDecodeT act (DecodeState bs 0)
  either fail pure ea

guardEnd :: Monad m => DecodeT m ()
guardEnd = do
  bs <- gets decStateInput
  unless (BSL.null bs) (fail "not end of input")

decodeBounded :: Monad m => ByteLength -> DecodeT m a -> DecodeT m a
decodeBounded len dec = do
  off <- gets decStateOffset
  let !end = off + unByteLength len
  elt <- dec
  off' <- gets decStateOffset
  if
    | off' > end -> fail ("consumed too much input: " ++ show (off' - end))
    | off' == end -> pure elt
    | otherwise -> fail ("consumed too little input: " ++ show (end - off'))

decodeFolded :: Monad m => ByteLength -> b -> (b -> DecodeT m b) -> DecodeT m b
decodeFolded len acc fdec = do
  off <- gets decStateOffset
  let !end = off + unByteLength len
  decodeFoldedUntil end acc fdec

decodeFoldedUntil :: Monad m => ByteOffset -> b -> (b -> DecodeT m b) -> DecodeT m b
decodeFoldedUntil end acc0 fdec = go acc0 where
  go !acc = do
    off' <- gets decStateOffset
    if
      | off' > end -> fail ("consumed too much input: " ++ show (off' - end))
      | off' == end -> pure acc
      | otherwise -> do
        acc' <- fdec acc
        go acc'

decodeRepeated :: Monad m => ByteLength -> DecodeT m a -> DecodeT m (Seq a)
decodeRepeated len dec = decodeFolded len Empty (\acc -> fmap (acc :|>) dec)

decodeGet :: Monad m => Get a -> DecodeT m a
decodeGet getter = do
  DecodeState bs off <- State.get
  case runGetOrFail getter bs of
    Left (bs', off', reason) -> do
      State.put (DecodeState bs' (off + off'))
      fail reason
    Right (bs', off', value) -> do
      State.put (DecodeState bs' (off + off'))
      pure value

getExpect :: (Eq a, Show a) => String -> Get a -> a -> Get ()
getExpect typ getter expec = do
  actual <- getter
  unless (expec == actual)
    (fail ("Expected " ++ " " ++ typ ++  " " ++ show expec ++ " but found " ++ show actual))

-- getInt24le :: Get Int24
-- getInt24le = get

-- putInt24le :: Int24 -> Put ()
-- putInt24le = undefined

getVecWith :: Prim a => Int -> Get a -> Get (VP.Vector a)
getVecWith len getter = VP.generateM len (const getter)

getVec :: (Prim a, Binary a) => Int -> Get (VP.Vector a)
getVec len = getVecWith len get

putVecWith :: Prim a => (a -> Put) -> VP.Vector a -> Put
putVecWith = VP.mapM_

putVec :: (Prim a, Binary a) => VP.Vector a -> Put
putVec = putVecWith put

getSeqWith :: Int -> Get a -> Get (Seq a)
getSeqWith = Seq.replicateA

getSeq :: Binary a => Int -> Get (Seq a)
getSeq len = getSeqWith len get

putSeqWith :: (a -> Put) -> Seq a -> Put
putSeqWith = traverse_

putSeq :: Binary a => Seq a -> Put
putSeq = putSeqWith put

class Binary a => SizedBinary a where
  byteSize :: a -> ByteLength

instance SizedBinary Int8 where
  byteSize = const 1

instance SizedBinary Word8 where
  byteSize = const 1

newtype BoolByte = BoolByte { unBoolByte :: Bool }
  deriving stock (Show)
  deriving newtype (Eq)

instance Binary BoolByte where
  get = fmap (BoolByte . (== 0)) getWord8
  put (BoolByte b) = putWord8 (if b then 1 else 0)

instance SizedBinary BoolByte where
  byteSize = const 1

instance Default BoolByte where
  def = BoolByte False

newtype FloatLE = FloatLE { unFloatLE :: Float }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Prim, Default)

instance Binary FloatLE where
  get = fmap FloatLE getFloatle
  put = putFloatle . unFloatLE

instance SizedBinary FloatLE where
  byteSize = const 4

newtype Word16LE = Word16LE { unWord16LE :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)

instance Binary Word16LE where
  get = fmap Word16LE getWord16le
  put = putWord16le . unWord16LE

instance SizedBinary Word16LE where
  byteSize = const 2

newtype Int16LE = Int16LE { unInt16LE :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)

instance Binary Int16LE where
  get = fmap Int16LE getInt16le
  put = putInt16le . unInt16LE

instance SizedBinary Int16LE where
  byteSize = const 2

newtype Word32LE = Word32LE { unWord32LE :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)

instance Binary Word32LE where
  get = fmap Word32LE getWord32le
  put = putWord32le . unWord32LE

instance SizedBinary Word32LE where
  byteSize = const 4

newtype Int32LE = Int32LE { unInt32LE :: Int32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)

instance Binary Int32LE where
  get = fmap Int32LE getInt32le
  put = putInt32le . unInt32LE

instance SizedBinary Int32LE where
  byteSize = const 4

newtype Word64LE = Word64LE { unWord64LE :: Word64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)

instance Binary Word64LE where
  get = fmap Word64LE getWord64le
  put = putWord64le . unWord64LE

instance SizedBinary Word64LE where
  byteSize = const 8

newtype Int64LE = Int64LE { unInt64LE :: Int64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)

instance Binary Int64LE where
  get = fmap Int64LE getInt64le
  put = putInt64le . unInt64LE

instance SizedBinary Int64LE where
  byteSize = const 8

getUntilNull :: Get ByteString
getUntilNull = fmap (BS.pack . toList) (go Empty) where
  go !acc = do
    w <- getWord8
    if w == 0
      then pure acc
      else go (acc :|> w)

newtype TermText = TermText { unTermText :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance Default TermText where
  def = TermText T.empty

instance Binary TermText where
  get = do
    bs <- getUntilNull
    unless (odd (BS.length bs)) $ do
      w <- getWord8
      unless (w == 0) (fail "term text missing word pad")
    pure $! TermText (TE.decodeLatin1 bs)

  put (TermText t) =
    let !bs0 = BSC.pack (T.unpack t)
        !bs1 = BS.snoc bs0 0
        !bs2 = if odd (BS.length bs1) then BS.snoc bs1 0 else bs1
    in putByteString bs2

instance SizedBinary TermText where
  byteSize (TermText t) =
    let len = fromIntegral (T.length t + 1)
    in if even len then len else len + 1

getFixedText :: ByteLength -> Get Text
getFixedText len = fmap (TE.decodeLatin1 . BS.takeWhile (/= 0)) (getByteString (fromIntegral len))

putFixedText :: ByteLength -> Text -> Put
putFixedText len t =
  let !intLen = fromIntegral len
      !bs0 = BSC.pack (take intLen (T.unpack t))
      !len0 = BS.length bs0
      !bs1 = if len0 < intLen then bs0 <> BS.replicate (intLen - len0) 0 else bs0
  in putByteString bs1

newtype FixedText (n :: Nat) = FixedText { unFixedText :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance Default (FixedText n) where
  def = FixedText T.empty

instance KnownNat n => Binary (FixedText n) where
  get = fmap FixedText (getFixedText (fromIntegral (natVal (Proxy :: Proxy n))))
  put ft@(FixedText t) = putFixedText (fromIntegral (natVal ft)) t

instance KnownNat n => SizedBinary (FixedText n) where
  byteSize = fromIntegral . natVal

putFixedBytes :: ByteLength -> ByteString -> Put
putFixedBytes len bs0 =
  let !intLen = fromIntegral len
      !len0 = BS.length bs0
      !bs1 = if len0 < intLen then bs0 <> BS.replicate (intLen - len0) 0 else bs0
  in putByteString bs1

newtype FixedBytes (n :: Nat) = FixedBytes { unFixedBytes :: ByteString }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance Default (FixedBytes n) where
  def = FixedBytes BS.empty

instance KnownNat n => Binary (FixedBytes n) where
  get = fmap FixedBytes (getByteString (fromIntegral (natVal (Proxy :: Proxy n))))
  put fb@(FixedBytes bs) = putFixedBytes (fromIntegral (natVal fb)) bs

instance KnownNat n => SizedBinary (FixedBytes n) where
  byteSize = fromIntegral . natVal
