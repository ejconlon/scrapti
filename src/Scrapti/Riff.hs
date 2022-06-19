{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Riff
  ( labelRiff
  , getLabel
  , expectLabel
  ) where

import Data.ByteString (ByteString)
import Scrapti.Binary (Get, getByteString, getExpect)

labelRiff :: ByteString
labelRiff = "RIFF"

getLabel :: Get ByteString
getLabel = getByteString 4

expectLabel :: ByteString -> Get ()
expectLabel = getExpect "label" getLabel
