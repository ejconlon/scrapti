module Scrapti.Binary where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Dahdit (ViaStaticByteSized (..), StaticByteSized (..), ByteSized, Binary (..), putByteString, getByteString, getExpect)
import Data.Proxy (Proxy (..))
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Short as BSS
import Data.Default (Default (..))

newtype ExactBytes (s :: Symbol) = ExactBytes { unExactBytes :: () }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (ByteSized) via (ViaStaticByteSized (ExactBytes s))

instance Default (ExactBytes s) where
  def = ExactBytes ()

instance KnownSymbol s => StaticByteSized (ExactBytes s) where
  staticByteSize _ = fromIntegral (length (symbolVal (Proxy :: Proxy s)))

instance KnownSymbol s => Binary (ExactBytes s) where
  get = do
    let !s = symbolVal (Proxy :: Proxy s)
        !bc = fromIntegral (length s)
        !bs = BSS.pack (fmap c2w s)
    getExpect s (getByteString bc) bs
    pure $! ExactBytes ()
  put _ = do
    let !s = symbolVal (Proxy :: Proxy s)
    putByteString (BSS.pack (fmap c2w s))
