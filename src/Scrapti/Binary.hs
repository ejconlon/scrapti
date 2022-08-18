module Scrapti.Binary
  ( QuietArray (..)
  ) where

import Dahdit (ByteSized (..), LiftedPrim, LiftedPrimArray, emptyLiftedPrimArray, sizeofLiftedPrimArray)
import Data.Default (Default (..))

newtype QuietArray a = QuietArray { unQuietArray :: LiftedPrimArray a }
  deriving newtype (Eq, ByteSized)

instance LiftedPrim a => Show (QuietArray a) where
  show (QuietArray arr) = "QuietArray{" ++ show (sizeofLiftedPrimArray arr) ++ "}"

instance Default (QuietArray a) where
  def = QuietArray emptyLiftedPrimArray
