module Scrapti.Parser.Sizes
  ( ElementCount (..)
  , ByteCount (..)
  , ByteSized (..)
  , StaticByteSized (..)
  , byteSizeViaStatic
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (Default)
import Data.Int (Int8)
import Data.Word (Word64, Word8)
import Scrapti.Parser.Proxy (Proxy (..), proxyFor)

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

instance ByteSized ByteString where
  byteSize = fromIntegral . BS.length

class ByteSized a => StaticByteSized a where
  staticByteSize :: Proxy a -> ByteCount

byteSizeViaStatic :: StaticByteSized a => a -> ByteCount
byteSizeViaStatic = staticByteSize . proxyFor

instance StaticByteSized Word8 where
  staticByteSize _ = 1

instance StaticByteSized Int8 where
  staticByteSize _ = 1
