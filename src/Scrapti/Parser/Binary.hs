module Scrapti.Parser.Binary
  ( Binary (..)
  , byteSizeViaPut
  ) where

import Scrapti.Parser.Free (Get, Put)
import Scrapti.Parser.Run (runCount)
import Scrapti.Parser.Sizes (ByteCount, ByteSized)

class ByteSized a => Binary a where
  get :: Get a
  put :: a -> Put

byteSizeViaPut :: Binary a => a -> ByteCount
byteSizeViaPut = runCount . put
