module Scrapti.Binary
  ( QuietArray (..)
  , QuietLiftedArray (..)
  )
where

import Dahdit (LiftedPrim, LiftedPrimArray, sizeofLiftedPrimArray)
import Data.Default (Default (..))
import Data.Primitive.ByteArray (ByteArray, emptyByteArray, sizeofByteArray)

newtype QuietArray = QuietArray {unQuietArray :: ByteArray}
  deriving newtype (Eq)

instance Show QuietArray where
  show (QuietArray arr) = "QuietArray{" ++ show (sizeofByteArray arr) ++ "}"

instance Default QuietArray where
  def = QuietArray emptyByteArray

newtype QuietLiftedArray a = QuietLiftedArray {unQuietLiftedArray :: LiftedPrimArray a}
  deriving newtype (Eq, Default)

instance LiftedPrim a => Show (QuietLiftedArray a) where
  show (QuietLiftedArray arr) = "QuietLiftedArray{" ++ show (sizeofLiftedPrimArray arr) ++ "}"
