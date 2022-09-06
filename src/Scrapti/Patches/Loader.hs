{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Patches.Loader
  ( Sample (..)
  , matchSamples
  , LoadedSample (..)
  , initializeInst
  , defaultInst
  ) where

import Data.Default (Default (..))
import Data.Maybe (maybeToList)
import Data.Ord (Down (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Scrapti.Common (LoopMarkNames, LoopMarks (..), SimpleMarker (..), defaultLoopMarkNames)
import Scrapti.Convert (Neutral (..), loadNeutral)
import Scrapti.Midi.Msg (Velocity (..))
import Scrapti.Midi.Notes (LinNote (..), OctNote, octToLin, parseNote)
import Scrapti.Patches.Inst (InstCrop (..), InstKeyRange (InstKeyRange), InstLoop (..),
                             InstLoopType (InstLoopTypeForward), InstRegion (..), InstSpec (..))
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~~))

data Sample = Sample
  { samplePath :: !FilePath
  , sampleNote :: !OctNote
  , sampleVel :: !(Maybe Velocity)
  , sampleUniq :: !(Maybe Text)
  } deriving stock (Eq, Show)

newtype F a = F { unF :: Either String a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail F where
  fail = F . Left

matchFiles :: Text -> Text -> FilePath -> IO [(FilePath, [Text])]
matchFiles prefix fileExt dir = do
  let pat = "^" ++ T.unpack prefix ++ "-([^\\\\\\.]+)\\." ++ T.unpack fileExt ++ "$"
  files <- listDirectory dir
  pure $! do
    fp <- files
    let ex = fp =~~ pat :: F (String, String, String, [String])
    case unF ex of
      Right (_, _, _, [dashed]) ->
        let parts = T.splitOn "-" (T.pack dashed)
        in [(dir </> fp, parts)]
      _ -> []

parseVel :: Text -> Maybe Velocity
parseVel v
  | not (T.null v) && T.head v == 'V' = fmap Velocity (readMaybe (T.unpack v))
  | otherwise = Nothing

parseSample :: FilePath -> [Text] -> Maybe Sample
parseSample fp parts = do
  (noteStr, mayVelStr, mayUniqStr) <- case parts of
    [noteStr] -> Just (noteStr, Nothing, Nothing)
    [noteStr, velStr] -> Just (noteStr, Just velStr, Nothing)
    [noteStr, velStr, uniqStr] -> Just (noteStr, Just velStr, Just uniqStr)
    _ -> Nothing
  octNote <- parseNote noteStr
  let mayVel = mayVelStr >>= parseVel
  pure $! Sample fp octNote mayVel mayUniqStr

matchSamples :: Text -> Text -> FilePath -> IO (Seq Sample)
matchSamples prefix fileExt dir = do
  pairs <- matchFiles prefix fileExt dir
  pure $! Seq.fromList $ pairs >>= maybeToList . uncurry parseSample

data LoadedSample = LoadedSample
  { lsPath :: !FilePath
  , lsContents :: !Neutral
  } deriving stock (Eq, Show)

loadSample :: Int -> Maybe LoopMarkNames -> Sample -> IO LoadedSample
loadSample sr mayNames s = do
  let fp = samplePath s
  ne <- loadNeutral sr mayNames fp
  pure $! LoadedSample fp ne

toRegion :: Int -> Maybe LoopMarkNames -> Sample -> InstKeyRange -> IO (InstRegion LoadedSample)
toRegion sr mayNames samp range = do
  ls <- loadSample sr mayNames samp
  let mayLoopMarks = neLoopMarks (lsContents ls)
      mayLoop = fmap (\(LoopMarks _ (_, start) (_, end) _) ->
        InstLoop InstLoopTypeForward (toInteger (smPosition start)) (toInteger (smPosition end))) mayLoopMarks
      mayCrop = fmap (\(LoopMarks (_, start) _ _ (_, end)) ->
        InstCrop (toInteger (smPosition start)) (toInteger (smPosition end))) mayLoopMarks
  pure $! InstRegion ls range mayLoop mayCrop

calcRangedSamps :: Seq Sample -> Seq (Sample, InstKeyRange)
calcRangedSamps = go 0 Empty where
  go !i !acc = \case
    Empty -> error "need more than one sample"
    s :<| Empty ->
      let n = toInteger (unLinNote (octToLin (sampleNote s)))
      in acc :|> (s, InstKeyRange i n 127)
    s :<| ss ->
      let n = toInteger (unLinNote (octToLin (sampleNote s)))
          i' = n + 1
          acc' = acc :|> (s, InstKeyRange i n n)
      in go i' acc' ss

-- TODO account for multi-samples of notes? Need to group by note
initializeInst :: Int -> Maybe LoopMarkNames -> Seq Sample -> IO (InstSpec LoadedSample)
initializeInst sr mayNames unordSamps = do
  let ordSamps = Seq.sortOn (Down . sampleNote) unordSamps
  regions <- traverse (uncurry (toRegion sr mayNames)) (calcRangedSamps ordSamps)
  pure $! InstSpec def regions

defaultInst :: Seq Sample -> IO (InstSpec LoadedSample)
defaultInst = initializeInst 44100 (Just defaultLoopMarkNames)
