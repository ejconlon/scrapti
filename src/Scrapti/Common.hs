{-# LANGUAGE OverloadedStrings #-}

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
  , UnparsedBody (..)
  , padCount
  , bssLast
  , bssInit
  , SimpleMarker (..)
  , dedupeSimpleMarkers
  , ConvertErr (..)
  , guardBps
  , guardChunk
  , LoopMarks (..)
  , LoopMarkNames
  , LoopMarkPoints
  , defaultLoopMarkNames
  , defineLoopMarks
  , findMark
  , findLoopMarks
  , recallLoopMarkNames
  , defaultNoteNumber
  , adjustMarker
  , adjustLoopPoints
  ) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Dahdit (Binary (..), ByteCount, ByteSized (..), Get, Put, StaticBytes, Word32BE, Word32LE, Word8, getExpect,
               getRemainingString, putByteString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Word (Word32)
import Scrapti.Dsp (DspErr)

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

data ConvertErr =
    ConvertErrMissingChunk !String
  | ConvertErrDsp !DspErr
  | ConvertErrBadBps !Int
  | ConvertErrMissingMark !ShortByteString
  | ConvertErrNoLoopMarks
  deriving stock (Eq, Show)

instance Exception ConvertErr

guardBps :: Int -> Int -> Either ConvertErr ()
guardBps needBps haveBps = unless (needBps == haveBps) (Left (ConvertErrBadBps haveBps))

guardChunk :: String -> Maybe c -> Either ConvertErr c
guardChunk name = maybe (Left (ConvertErrMissingChunk name)) Right

data LoopMarks a = LoopMarks
  { lmStart :: !a
  , lmLoopStart :: !a
  , lmLoopEnd :: !a
  , lmEnd :: !a
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

type LoopMarkNames = LoopMarks ShortByteString
type LoopMarkPoints = LoopMarks (Int, SimpleMarker)

defaultLoopMarkNames :: LoopMarkNames
defaultLoopMarkNames = LoopMarks "Start" "LoopStart" "LoopEnd" "End"

defineLoopMarks :: Integral a => LoopMarkNames -> LoopMarks a -> LoopMarkPoints
defineLoopMarks (LoopMarks nw nx ny nz) (LoopMarks pw px py pz) = LoopMarks
  (0, SimpleMarker nw (fromIntegral pw))
  (1, SimpleMarker nx (fromIntegral px))
  (2, SimpleMarker ny (fromIntegral py))
  (3, SimpleMarker nz (fromIntegral pz))

findMark :: ShortByteString -> Seq SimpleMarker -> Either ConvertErr (Int, SimpleMarker)
findMark name marks =
  case Seq.findIndexL (\sm -> smName sm == name) marks of
    Nothing -> Left (ConvertErrMissingMark name)
    Just ix -> Right (ix, Seq.index marks ix)

findLoopMarks :: LoopMarkNames -> Seq SimpleMarker -> Either ConvertErr LoopMarkPoints
findLoopMarks names marks = traverse (`findMark` marks) names

recallLoopMarkNames :: LoopMarkPoints -> LoopMarkNames
recallLoopMarkNames = fmap (smName . snd)

-- | Midi note C5 is default note
defaultNoteNumber :: Int
defaultNoteNumber = 72

adjustMarker :: Word32 -> SimpleMarker -> SimpleMarker
adjustMarker amt (SimpleMarker name pos) = SimpleMarker name (amt + pos)

adjustLoopPoints :: Word32 -> LoopMarkPoints -> LoopMarkPoints
adjustLoopPoints = fmap . fmap . adjustMarker
