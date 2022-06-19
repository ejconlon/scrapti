module Scrapti.Sfont where

import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word32)
import Scrapti.Wav (Wav)

data Sfont a = Sfont
  { sfontInfos :: !(Seq Info)
  , sfontSdata :: !(Wav a)
  } deriving stock (Eq, Show)

data Info =
    InfoVersion !Word32 !Word32
  | InfoTargetSoundEngine !Text
  | InfoBankName !Text
  | InfoRomName !Text
  | InfoRomVersion !Word32 !Word32
  | InfoCreationDate !Text
  | InfoAuthors !Text
  | InfoIntendedProduct !Text
  | InfoCopyrightMessage !Text
  | InfoComments !Text
  | InfoUsedTools !Text
  | InfoReservedInfo !Text !Word32 !ByteString
  deriving stock (Eq, Show)
