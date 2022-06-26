module Dahdit.Binary
  ( Binary (..)
  , byteSizeViaPut
  ) where

import Dahdit.Free (Get, Put)
import Dahdit.Funs (getInt16LE, getInt8, getWord16LE, getWord8, putInt16LE, putInt8, putWord16LE, putWord8)
import Dahdit.Nums (Int16LE, Word16LE)
import Dahdit.Run (runCount)
import Dahdit.Sizes (ByteCount, ByteSized)
import Data.Int (Int8)
import Data.Word (Word8)

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
