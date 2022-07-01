module Scrapti.Binary
  ( QuietArray (..)
  ) where

import Dahdit (ByteSized)
import Data.Default (Default (..))
import Data.Primitive (Prim)
import Data.Primitive.PrimArray (PrimArray, emptyPrimArray, sizeofPrimArray)

newtype QuietArray a = QuietArray { unQuietArray :: PrimArray a }
  deriving newtype (Eq, ByteSized)

instance Prim a => Show (QuietArray a) where
  show (QuietArray arr) = "QuietArray{" ++ show (sizeofPrimArray arr) ++ "}"

instance Default (QuietArray a) where
  def = QuietArray emptyPrimArray
