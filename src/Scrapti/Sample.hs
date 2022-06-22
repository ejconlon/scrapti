module Scrapti.Sample
  ( Sample (..)
  , Sampled (..)
  , getSampled
  ) where

import Data.Int (Int8)
import Data.Proxy (Proxy)
import Data.Word (Word16)
import Scrapti.Binary (Get, Binary (..), Int16LE, Int32LE, Int64LE)
import Data.Primitive (Prim)

class (Prim a, Binary a) => Sample a where
  sampleBits :: Proxy a -> Int
  sampleBytes :: Proxy a -> Int
  sampleBytes p = div (sampleBits p) 8

instance Sample Int8 where
  sampleBits _ = 8

instance Sample Int16LE where
  sampleBits _ = 16

-- TODO find a good 24-bit int wrapper
-- instance Sample Int24LE where
--   sampleBits _ = 24

instance Sample Int32LE where
  sampleBits _ = 32

instance Sample Int64LE where
  sampleBits _ = 64

data Sampled f where
  Sampled :: Sample a => !(f a) -> Sampled f

getSampled :: Word16 -> Maybe (Sampled Get)
getSampled = \case
  8 -> Just (Sampled (get :: Get Int8))
  16 -> Just (Sampled (get :: Get Int16LE))
  -- 24 -> Just (Sampled (sampleGet :: Get Int24LE))
  32 -> Just (Sampled (get :: Get Int32LE))
  64 -> Just (Sampled (get :: Get Int64LE))
  _ -> Nothing
