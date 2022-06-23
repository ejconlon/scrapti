{-# LANGUAGE UnboxedTuples #-}

module Scrapti.Binary
  ( Binary (..)
  , Get
  , Put
  , runPut
  , ByteLength (..)
  , ByteOffset
  , DecodeState (..)
  , DecodeM
  , runDecodeM
  , decodeFail
  , decodeGet
  , guardEnd
  , decodeBounded
  , decodeFolded
  , decodeRepeated
  , ParseM
  , parseSized
  , parseRepeated
  , runParseM
  , ByteSized (..)
  , StaticByteSized (..)
  , WithByteSize (..)
  , BinarySized (..)
  , ViaStaticByteSized (..)
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
  , FixedVec (..)
  , getByteString
  , getExpect
  , getVec
  , getVecWith
  , getSeq
  , getSeqWith
  -- , getRepeated
  , skip
  , putByteString
  , putVec
  , putVecWith
  , putSeq
  , putSeqWith
  ) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State.Strict (MonadState, State, StateT (..), gets, runState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (lift)
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
import Data.Foldable (foldMap', toList, traverse_)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive (Prim)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum (..))
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

newtype DecodeM a = DecodeM { unDecodeM :: ExceptT String (State DecodeState) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DecodeState)

instance MonadFail DecodeM where
  fail = DecodeM . throwError

runDecodeM :: DecodeM a -> DecodeState -> (Either String a, DecodeState)
runDecodeM dm = runState (runExceptT (unDecodeM dm))

decodeFail :: MonadFail m => BSL.ByteString -> DecodeM a -> m a
decodeFail bs act =
  let (ea, _) = runDecodeM act (DecodeState bs 0)
  in either fail pure ea

guardEnd :: DecodeM ()
guardEnd = do
  bs <- gets decStateInput
  unless (BSL.null bs) (fail "not end of input")

decodeBounded :: ByteLength -> DecodeM a -> DecodeM a
decodeBounded len dec = do
  off <- gets decStateOffset
  let !end = off + unByteLength len
  elt <- dec
  off' <- gets decStateOffset
  if
    | off' > end -> fail ("consumed too much input: " ++ show (off' - end))
    | off' == end -> pure elt
    | otherwise -> fail ("consumed too little input: " ++ show (end - off'))

decodeFolded :: ByteLength -> b -> (b -> DecodeM b) -> DecodeM b
decodeFolded len acc fdec = do
  off <- gets decStateOffset
  let !end = off + unByteLength len
  decodeFoldedUntil end acc fdec

decodeFoldedUntil :: ByteOffset -> b -> (b -> DecodeM b) -> DecodeM b
decodeFoldedUntil end acc0 fdec = go acc0 where
  go !acc = do
    off' <- gets decStateOffset
    if
      | off' > end -> fail ("consumed too much input: " ++ show (off' - end))
      | off' == end -> pure acc
      | otherwise -> do
        acc' <- fdec acc
        go acc'

decodeRepeated :: ByteLength -> DecodeM a -> DecodeM (Seq a)
decodeRepeated len dec = decodeFolded len Empty (\acc -> fmap (acc :|>) dec)

decodeGet :: Get a -> DecodeM a
decodeGet getter = do
  DecodeState bs off <- State.get
  case runGetOrFail getter bs of
    Left (bs', off', reason) -> do
      State.put (DecodeState bs' (off + off'))
      fail reason
    Right (bs', off', value) -> do
      State.put (DecodeState bs' (off + off'))
      pure value

newtype ParseM a = ParseM { unParseM :: StateT ByteLength Get a }
  deriving newtype (Functor, Applicative, Monad, MonadFail)

parseSized :: BinarySized a => ParseM a
parseSized = do
  left <- ParseM State.get
  WithByteSize size value <- ParseM (lift getSized)
  let !newLeft = left - size
  if newLeft < 0
    then fail ("Consumed too much input: " ++ show newLeft)
    else value <$ ParseM (State.put newLeft)

parseRepeated :: BinarySized a => ParseM (Seq a)
parseRepeated = go Empty where
  go !acc = do
    left <- ParseM State.get
    if left == 0
      then pure acc
      else do
        a <- parseSized
        go (acc :|> a)

runParseM :: ByteLength -> ParseM a -> Get (WithByteSize a)
runParseM len pm = do
  (!value, !left) <- runStateT (unParseM pm) len
  unless (left == 0) (fail "Not end of input")
  pure $! WithByteSize len value

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

-- getRepeated :: BinarySized a => ByteLength -> Get (Seq a)
-- getRepeated = go Empty where
--   go !acc !left = do
--     if
--       | left == 0 -> pure acc
--       | left < 0 -> fail ("consumed too much input by: " ++ show left ++ " for number of elements: " ++ show (Seq.length acc))
--       | otherwise -> do
--         WithByteSize sz a <- getSized
--         go (acc :|> a) (left - sz)

class ByteSized a where
  byteSize :: a -> ByteLength

instance ByteSized Int8 where
  byteSize = const 1

instance ByteSized Word8 where
  byteSize = const 1

instance ByteSized a => ByteSized (Seq a) where
  byteSize = getSum . foldMap' (Sum . byteSize)

instance (ByteSized a, Prim a) => ByteSized (VP.Vector a) where
  byteSize = getSum . VP.foldMap' (Sum . byteSize)

class ByteSized a => StaticByteSized a where
  staticByteSize :: Proxy a -> ByteLength

instance StaticByteSized Int8 where
  staticByteSize = const 1

instance StaticByteSized Word8 where
  staticByteSize = const 1

data WithByteSize a = WithByteSize !ByteLength !a
  deriving stock (Eq, Show)

instance (ByteSized a, Default a) => Default (WithByteSize a) where
  def = let a = def in WithByteSize (byteSize a) a

class (ByteSized a, Binary a) => BinarySized a where
  getSized :: Get (WithByteSize a)
  getSized = fmap (\a -> WithByteSize (byteSize a) a) get

newtype ViaStaticByteSized a = ViaStaticByteSized { unStaticByteSized :: a }
  deriving newtype (Binary)

instance StaticByteSized a => ByteSized (ViaStaticByteSized a) where
  byteSize = const (staticByteSize (Proxy :: Proxy a))

instance (StaticByteSized a, Binary a) => BinarySized (ViaStaticByteSized a) where
  getSized = fmap (WithByteSize (staticByteSize (Proxy :: Proxy a)) . ViaStaticByteSized) get

newtype BoolByte = BoolByte { unBoolByte :: Bool }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized BoolByte)

instance Binary BoolByte where
  get = fmap (BoolByte . (== 0)) getWord8
  put (BoolByte b) = putWord8 (if b then 1 else 0)

instance StaticByteSized BoolByte where
  staticByteSize = const 1

instance Default BoolByte where
  def = BoolByte False

newtype FloatLE = FloatLE { unFloatLE :: Float }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Prim, Default)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized FloatLE)

instance Binary FloatLE where
  get = fmap FloatLE getFloatle
  put = putFloatle . unFloatLE

instance StaticByteSized FloatLE where
  staticByteSize = const 4

newtype Word16LE = Word16LE { unWord16LE :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized Word16LE)

instance Binary Word16LE where
  get = fmap Word16LE getWord16le
  put = putWord16le . unWord16LE

instance StaticByteSized Word16LE where
  staticByteSize = const 2

newtype Int16LE = Int16LE { unInt16LE :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized Int16LE)

instance Binary Int16LE where
  get = fmap Int16LE getInt16le
  put = putInt16le . unInt16LE

instance StaticByteSized Int16LE where
  staticByteSize = const 2

newtype Word32LE = Word32LE { unWord32LE :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized Word32LE)

instance Binary Word32LE where
  get = fmap Word32LE getWord32le
  put = putWord32le . unWord32LE

instance StaticByteSized Word32LE where
  staticByteSize = const 4

newtype Int32LE = Int32LE { unInt32LE :: Int32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized Int32LE)

instance Binary Int32LE where
  get = fmap Int32LE getInt32le
  put = putInt32le . unInt32LE

instance StaticByteSized Int32LE where
  staticByteSize = const 4

newtype Word64LE = Word64LE { unWord64LE :: Word64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized Word64LE)

instance Binary Word64LE where
  get = fmap Word64LE getWord64le
  put = putWord64le . unWord64LE

instance StaticByteSized Word64LE where
  staticByteSize = const 8

newtype Int64LE = Int64LE { unInt64LE :: Int64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized Int64LE)

instance Binary Int64LE where
  get = fmap Int64LE getInt64le
  put = putInt64le . unInt64LE

instance StaticByteSized Int64LE where
  staticByteSize = const 8

getUntilNull :: Get ByteString
getUntilNull = fmap (BS.pack . toList) (go Empty) where
  go !acc = do
    w <- getWord8
    if w == 0
      then pure acc
      else go (acc :|> w)

-- | Text terminated with null byte
-- NOTE Terminated with TWO null bytes if the string is even length
-- to align to Word16 boundaries
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

instance ByteSized TermText where
  byteSize (TermText t) =
    let len = fromIntegral (T.length t + 1)
    in if even len then len else len + 1

-- Use the default impl - could be better, but doesn't really matter
instance BinarySized TermText

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
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized (FixedText n))

instance Default (FixedText n) where
  def = FixedText T.empty

instance KnownNat n => Binary (FixedText n) where
  get = fmap FixedText (getFixedText (fromIntegral (natVal (Proxy :: Proxy n))))
  put ft@(FixedText t) = putFixedText (fromIntegral (natVal ft)) t

instance KnownNat n => StaticByteSized (FixedText n) where
  staticByteSize = const (fromIntegral (natVal (Proxy :: Proxy n)))

putFixedBytes :: ByteLength -> ByteString -> Put
putFixedBytes len bs0 =
  let !intLen = fromIntegral len
      !len0 = BS.length bs0
      !bs1 = if len0 < intLen then bs0 <> BS.replicate (intLen - len0) 0 else bs0
  in putByteString bs1

newtype FixedBytes (n :: Nat) = FixedBytes { unFixedBytes :: ByteString }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized (FixedBytes n))

instance Default (FixedBytes n) where
  def = FixedBytes BS.empty

instance KnownNat n => Binary (FixedBytes n) where
  get = fmap FixedBytes (getByteString (fromIntegral (natVal (Proxy :: Proxy n))))
  put fb@(FixedBytes bs) = putFixedBytes (fromIntegral (natVal fb)) bs

instance KnownNat n => StaticByteSized (FixedBytes n) where
  staticByteSize = const (fromIntegral (natVal (Proxy :: Proxy n)))

newtype FixedVec (n :: Nat) a = FixedVec { unFixedVec :: VP.Vector a }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (ByteSized, BinarySized) via (ViaStaticByteSized (FixedVec n a))

instance (KnownNat n, Prim a, Default a) => Default (FixedVec n a) where
  def = FixedVec (VP.replicate (fromIntegral (natVal (Proxy :: Proxy n))) def)

instance (KnownNat n, Prim a, Binary a) => Binary (FixedVec n a) where
  get = fmap FixedVec (getVec (fromIntegral (natVal (Proxy :: Proxy n))))
  put (FixedVec v0) =
    let !intLen = fromIntegral (natVal (Proxy :: Proxy n))
        !v1 = if VP.length v0 == intLen then v0 else VP.take intLen v0
    in putVec v1

instance (KnownNat n, StaticByteSized a) => StaticByteSized (FixedVec n a) where
  staticByteSize = const (fromIntegral (natVal (Proxy :: Proxy n)) * staticByteSize (Proxy :: Proxy a))
