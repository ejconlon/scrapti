module Scrapti.Tracker.Checked
  ( Checked (..)
  , mkCode
  , mkChecked
  , updateCheckedCode
  , verifyCheckedCode
  ) where

import Dahdit (Binary, ByteSized, StaticByteSized, ViaStaticGeneric (..), Word32LE, put, runPut)
import qualified Data.ByteString.Short as BSS
import Data.Default (Default (..))
import Data.Digest.CRC32 (crc32)
import GHC.Generics (Generic)

data Checked a = Checked
  { checkedVal :: !a
  , checkedCode :: !Word32LE
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric (Checked a))

instance (Default a, Binary a) => Default (Checked a) where
  def = mkChecked def

mkCode :: Binary a => a -> Word32LE
mkCode val = fromIntegral (crc32 (BSS.fromShort (runPut (put val))))

mkChecked :: Binary a => a -> Checked a
mkChecked a = Checked a (mkCode a)

updateCheckedCode :: Binary a => Checked a -> Checked a
updateCheckedCode = mkChecked . checkedVal

verifyCheckedCode :: Binary a => Checked a -> Bool
verifyCheckedCode (Checked val code) = code == mkCode val
