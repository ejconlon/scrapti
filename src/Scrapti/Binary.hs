module Scrapti.Binary
  ( Get
  , Put
  , ByteOffset
  , DecodeError (..)
  , DecodeSuccess (..)
  , DecodeResult
  , decode
  , getExpect
  , getWord16le
  , getWord32le
  , getInt8
  , getInt16le
  , getInt32le
  , getInt64le
  , getByteString
  , getVec
  , skip
  , putWord16le
  , putWord32le
  , putInt8
  , putInt16le
  , putInt32le
  , putInt64le
  , putByteString
  , putVec
  ) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Binary.Get (ByteOffset, Get, getByteString, getInt16le, getInt32le, getInt64le, getInt8, getWord16le,
                        getWord32le, runGetOrFail, skip)
import Data.Binary.Put (Put, putByteString, putInt16le, putInt32le, putInt64le, putInt8, putWord16le, putWord32le)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Unboxed as VU

data DecodeError = DecodeError
  { deOffset :: !ByteOffset
  , deReason :: !String
  } deriving stock (Eq, Show)

instance Exception DecodeError

data DecodeSuccess a = DecodeSuccess
  { dsOffset :: !ByteOffset
  , dsValue :: !a
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

type DecodeResult a = Either DecodeError (DecodeSuccess a)

decode :: BSL.ByteString -> Get a -> DecodeResult a
decode bs getter = case runGetOrFail getter bs of
  Left (_, off, reason) -> Left (DecodeError off reason)
  Right (_, off, value) -> Right (DecodeSuccess off value)

getExpect :: (Eq a, Show a) => String -> Get a -> a -> Get ()
getExpect typ get expec = do
  actual <- get
  unless (expec == actual)
    (fail ("Expected " ++ " " ++ typ ++  " " ++ show expec ++ " but found " ++ show actual))

-- getInt24le :: Get Int24
-- getInt24le = get

getVec :: VU.Unbox a => Int -> Get a -> Get (VU.Vector a)
getVec len getter = VU.generateM len (const getter)

-- putInt24le :: Int24 -> Put ()
-- putInt24le = undefined

putVec :: VU.Unbox a => (a -> Put) -> VU.Vector a -> Put
putVec = VU.foldMap'
