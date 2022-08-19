module Scrapti.Common
  ( Label
  , labelSize
  , countSize
  , getExpectLabel
  , getChunkSizeLE
  , expectChunkSizeLE
  , putChunkSizeLE
  , getChunkSizeBE
  , expectChunkSizeBE
  , putChunkSizeBE
  , chunkHeaderSize
  , KnownLabel (..)
  , Sampled (..)
  , getSampled
  , UnparsedBody (..)
  , padCount
  , bssLast
  , bssInit
  , SimpleMarker (..)
  , dedupeSimpleMarkers
  ) where

import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, Int16LE, Int24LE, Int32LE, Int8, LiftedPrim, Put,
               StaticByteSized, StaticBytes, Word32BE, Word32LE, Word8, getExpect, getRemainingString, putByteString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Word (Word32)

type Label = StaticBytes 4

labelSize, countSize :: ByteCount
labelSize = 4
countSize = 4

getExpectLabel :: Label -> Get ()
getExpectLabel = getExpect "label" get

getChunkSizeLE :: Get ByteCount
getChunkSizeLE = fmap fromIntegral (get @Word32LE)

expectChunkSizeLE :: ByteCount -> Get ()
expectChunkSizeLE = getExpect "chunk size" getChunkSizeLE

putChunkSizeLE :: ByteCount -> Put
putChunkSizeLE = put @Word32LE . fromIntegral

getChunkSizeBE :: Get ByteCount
getChunkSizeBE = fmap fromIntegral (get @Word32BE)

expectChunkSizeBE :: ByteCount -> Get ()
expectChunkSizeBE = getExpect "chunk size" getChunkSizeBE

putChunkSizeBE :: ByteCount -> Put
putChunkSizeBE = put @Word32BE . fromIntegral

chunkHeaderSize :: ByteCount
chunkHeaderSize = 8

class KnownLabel a where
  knownLabel :: Proxy a -> Label

data Sampled f where
  Sampled :: (LiftedPrim a, Binary a, StaticByteSized a) => !(f a) -> Sampled f

getSampled :: Int -> Maybe (Sampled Proxy)
getSampled = \case
  8 -> Just (Sampled (Proxy :: Proxy Int8))
  16 -> Just (Sampled (Proxy :: Proxy Int16LE))
  24 -> Just (Sampled (Proxy :: Proxy Int24LE))
  32 -> Just (Sampled (Proxy :: Proxy Int32LE))
  _ -> Nothing

newtype UnparsedBody = UnparsedBody
  { ubContents :: ShortByteString
  } deriving stock (Show)
    deriving newtype (Eq)

instance ByteSized UnparsedBody where
  byteSize (UnparsedBody bs) = byteSize bs

instance Binary UnparsedBody where
  get = fmap UnparsedBody getRemainingString
  put (UnparsedBody bs) = putByteString bs

padCount :: ByteCount -> ByteCount
padCount bc = if even bc then bc else bc + 1

-- NOTE: Remove this when BS lib is updated
bssLast :: ShortByteString -> Word8
-- bssLast = BSS.last
bssLast sbs = BSS.index sbs (BSS.length sbs - 1)

-- NOTE: Remove this when BS lib is updated
bssInit :: ShortByteString -> ShortByteString
-- bssInit = BSS.init
bssInit = BSS.pack . init . BSS.unpack

data SimpleMarker = SimpleMarker
  { smName :: !ShortByteString
  -- ^ name of the cue point
  , smPosition :: !Word32
  -- ^ position in SAMPLES not bytes or elements
  } deriving stock (Eq, Show)

ordNubBy :: Ord b => (a -> b) -> Seq a -> Seq a
ordNubBy f = go Set.empty Seq.empty where
  go !accSet !accSeq = \case
    Empty -> accSeq
    x :<| xs ->
      let !y = f x
      in if Set.member y accSet
        then go accSet accSeq xs
        else go (Set.insert y accSet) (accSeq :|> x) xs

-- | Sort, keeping the first of any given name
dedupeSimpleMarkers :: Seq SimpleMarker -> Seq SimpleMarker
dedupeSimpleMarkers = ordNubBy smName . Seq.sortOn smPosition
