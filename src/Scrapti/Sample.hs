module Scrapti.Sample
  ( Sample (..)
  , Sampled (..)
  , getSampled
  ) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word16)
import Scrapti.Binary (Get, Put, getInt16le, getInt32le, getInt64le, getInt8, putInt16le, putInt32le, putInt64le,
                       putInt8)

class VU.Unbox a => Sample a where
  sampleGet :: Get a
  samplePut :: a -> Put
  sampleBits :: Proxy a -> Int
  sampleBytes :: Proxy a -> Int
  sampleBytes p = div (sampleBits p) 8

instance Sample Int8 where
  sampleGet = getInt8
  samplePut = putInt8
  sampleBits _ = 8

instance Sample Int16 where
  sampleGet = getInt16le
  samplePut = putInt16le
  sampleBits _ = 16

-- TODO find a good 24-bit int wrapper
-- instance Sample Int24 where
--   sampleGet = getInt24le
--   samplePut = putInt24le
--   sampleBits _ = 24

instance Sample Int32 where
  sampleGet = getInt32le
  samplePut = putInt32le
  sampleBits _ = 32

instance Sample Int64 where
  sampleGet = getInt64le
  samplePut = putInt64le
  sampleBits _ = 64

data Sampled f where
  Sampled :: Sample a => !(f a) -> Sampled f

getSampled :: Word16 -> Maybe (Sampled Get)
getSampled = \case
  8 -> Just (Sampled (sampleGet :: Get Int8))
  16 -> Just (Sampled (sampleGet :: Get Int16))
  -- 24 -> Just (Sampled (sampleGet :: Get Int24))
  32 -> Just (Sampled (sampleGet :: Get Int32))
  64 -> Just (Sampled (sampleGet :: Get Int64))
  _ -> Nothing
