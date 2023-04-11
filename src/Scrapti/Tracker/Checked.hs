module Scrapti.Tracker.Checked
  ( Checked (..)
  , mkCode
  , mkChecked
  , updateCheckedCode
  , verifyCheckedCode
  , failCheckedCode
  )
where

import Control.Monad (unless)
import Dahdit (Binary, ByteSized, StaticByteSized, ViaStaticGeneric (..), Word32LE (..), encode)
import Data.ByteString (ByteString)
import Data.Default (Default (..))
import Data.Digest.CRC32 (crc32)
import GHC.Generics (Generic)

data Checked a = Checked
  { checkedVal :: !a
  , checkedCode :: !Word32LE
  }
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric (Checked a))

instance (Default a, Binary a, ByteSized a) => Default (Checked a) where
  def = mkChecked def

mkCode :: (Binary a, ByteSized a) => a -> Word32LE
mkCode val = fromIntegral (crc32 @ByteString (encode val))

mkChecked :: (Binary a, ByteSized a) => a -> Checked a
mkChecked a = Checked a (mkCode a)

updateCheckedCode :: (Binary a, ByteSized a) => Checked a -> Checked a
updateCheckedCode = mkChecked . checkedVal

verifyCheckedCode :: (Binary a, ByteSized a) => Checked a -> Bool
verifyCheckedCode (Checked val code) = code == mkCode val

failCheckedCode :: (MonadFail m, Binary a, ByteSized a) => String -> Checked a -> m ()
failCheckedCode tyName (Checked val code) =
  let actual = mkCode val
  in  unless
        (code == actual)
        (fail ("Code for " ++ tyName ++ " failed to check (expected: " ++ show (unWord32LE code) ++ ", actual: " ++ show (unWord32LE actual) ++ ")"))
