{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Patch where
import Control.Monad (unless)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Sequence (Seq)
import Data.Text (Text)
import Scrapti.Common (LoopMarkNames, LoopMarkPoints)
import Scrapti.Convert (readPtiWav)
import Scrapti.Patches.Inst (InstSpec)
import Scrapti.Tracker.Pti (Header, InstParams, Pti, mkPti)

noteMapping :: Map Text Int
noteMapping = Map.fromList
  [ ("C0", 12)
  , ("C1", 24)
  , ("C2", 36)
  , ("C3", 48)
  , ("C4", 60)
  , ("C5", 72)
  , ("C6", 84)
  , ("C7", 96)
  , ("C8", 108)
  , ("C9", 120)
  ]

data Sample = Sample
  { samplePath :: !FilePath
  , sampleNote :: !Int
  , sampleVel :: !(Maybe Int)
  , sampleUniq :: !(Maybe Text)
  } deriving stock (Eq, Show)

findSamples :: Text -> FilePath -> IO (Seq Sample)
findSamples prefix dir = error "TODO"

data PtiInst = PtiInst !Text !Text !Pti
  deriving (Eq, Show)

instToHeader :: Maybe LoopMarkPoints -> Int -> InstParams -> Either String Header
instToHeader mayPoints noteOffset params = do
  let !hd = def @Header
  error "TODO"

instToPti :: Maybe LoopMarkNames -> Text -> Text -> Int -> InstParams -> Sample -> IO Pti
instToPti mayNames instName noteName noteCenterVal params sample = do
  (arr, mayPoints) <- readPtiWav mayNames (samplePath sample)
  unless (isNothing mayNames || isJust mayPoints) (fail "did not find expected loop points")
  let !header = undefined
  pure $! mkPti header arr

instToPtis :: InstSpec FilePath -> Seq Sample -> IO (Seq PtiInst)
instToPtis = error "TODO"

-- instToSfz :: InstSpec -> Seq Sample -> IO SfzFile
-- instToSfz = error "TODO"
