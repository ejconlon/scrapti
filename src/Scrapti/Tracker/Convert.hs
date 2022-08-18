module Scrapti.Tracker.Convert
  ( ConvertOpts (..)
  -- , convertSfont
  ) where

import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Writer.Strict (MonadWriter (..), Writer, runWriter)
import Dahdit (Int16LE (..), LiftedPrimArray, Word32LE (..))
import Data.Default (Default (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Scrapti.Tracker.Pti (Pti)

data ConvertOpts = ConvertOpts
  deriving stock (Eq, Show)

instance Default ConvertOpts where
  def = ConvertOpts

data ConvertEnv = ConvertEnv
  { ceOpts :: !ConvertOpts
  , ceSamples :: !(LiftedPrimArray Int16LE)
  } deriving stock (Eq, Show)

data NamedPti = NamedPti !FilePath !Pti
  deriving stock (Eq, Show)

newtype ConvertM a = ConvertM { unConvertM :: ReaderT ConvertEnv (Writer (Seq NamedPti)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader ConvertEnv, MonadWriter (Seq NamedPti))

emitPti :: FilePath -> Pti -> ConvertM ()
emitPti fp pti = tell (Seq.singleton (NamedPti fp pti))

runConvert :: ConvertM a -> ConvertEnv -> (a, Seq NamedPti)
runConvert m opts = runWriter (runReaderT (unConvertM m) opts)

data NicePreset = NicePreset
  { npName :: !String
  , npBank :: !Int
  , npZones :: !(Seq NiceZone)
  } deriving stock (Eq, Show)

data NiceZone = NiceZone
  {
  } deriving stock (Eq, Show)

data NiceInst = NiceInst
  { niName :: !String
  , niZones :: !(Seq NiceZone)
  } deriving stock (Eq, Show)

data NiceSample = NiceSample
  { nsName :: !String
  , nsStart :: !Word32LE
  , nsLoopStart :: !Word32LE
  , nsLoopEnd :: !Word32LE
  , nsEnd :: !Word32LE
  } deriving stock (Eq, Show)

-- convertSfontM :: Sfont -> ConvertM ()
-- convertSfontM _sfont = pure () -- TODO

-- convertSfont :: ConvertOpts -> Sfont -> Seq NamedPti
-- convertSfont opts sfont = snd (runConvertM (convertSfontM sfont) opts)
