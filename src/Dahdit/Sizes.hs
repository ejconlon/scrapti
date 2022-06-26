module Dahdit.Sizes
  ( ElementCount (..)
  , ByteCount (..)
  , ByteSized (..)
  , StaticByteSized (..)
  , byteSizeViaStatic
  ) where

import Dahdit.Proxy (Proxy (..), proxyFor)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Default (Default)
import Data.Int (Int8)
import Data.Word (Word64, Word8)

newtype ElementCount = ElementCount { unElementCount :: Word64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bounded, Default)

newtype ByteCount = ByteCount { unByteCount :: Word64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bounded, Default)

class ByteSized a where
  byteSize :: a -> ByteCount

instance ByteSized Word8 where
  byteSize _ = 1

instance ByteSized Int8 where
  byteSize _ = 1

instance ByteSized ShortByteString where
  byteSize = fromIntegral . BSS.length

class ByteSized a => StaticByteSized a where
  staticByteSize :: Proxy a -> ByteCount

byteSizeViaStatic :: StaticByteSized a => a -> ByteCount
byteSizeViaStatic = staticByteSize . proxyFor

instance StaticByteSized Word8 where
  staticByteSize _ = 1

instance StaticByteSized Int8 where
  staticByteSize _ = 1
