module Scrapti.Parser.Binary
  ( Binary (..)
  , byteSizeViaPut
  ) where

import Data.Int (Int8)
import Data.Word (Word8)
import Scrapti.Parser.Free (Get, Put)
import Scrapti.Parser.Funs (getInt16LE, getInt8, getWord16LE, getWord8, putInt16LE, putInt8, putWord16LE, putWord8)
import Scrapti.Parser.Nums (Int16LE, Word16LE)
import Scrapti.Parser.Run (runCount)
import Scrapti.Parser.Sizes (ByteCount, ByteSized)

class ByteSized a => Binary a where
  get :: Get a
  put :: a -> Put

byteSizeViaPut :: Binary a => a -> ByteCount
byteSizeViaPut = runCount . put

instance Binary Word8 where
  get = getWord8
  put = putWord8

instance Binary Int8 where
  get = getInt8
  put = putInt8

instance Binary Word16LE where
  get = getWord16LE
  put = putWord16LE

instance Binary Int16LE where
  get = getInt16LE
  put = putInt16LE
