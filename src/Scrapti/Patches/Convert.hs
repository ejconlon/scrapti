{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scrapti.Patches.Convert
  ( PatchErr (..)
  , sfzToInst
  , SfzSample (..)
  , instToSfz
  ) where

import Control.Exception (Exception)
import Control.Monad (join, unless, (>=>))
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.Default (Default (..))
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Scrapti.Patches.Inst (InstKeyRange (..), InstLfoWave (..), InstLoop (..), InstLoopType (..), InstParams (..),
                             InstRegion (..), InstSpec (..))
import Scrapti.Patches.Sfz (SfzAttrs, SfzFile (..), SfzSection (..), SfzVal (..), sfzValFloat, sfzValInt, sfzValText,
                            textSfzVal)


renderWaveNum :: InstLfoWave -> Int
renderWaveNum = \case
  InstLfoRevSaw -> 7
  InstLfoSaw -> 6
  InstLfoTriangle -> 0
  InstLfoSquare -> 3
  InstLfoRandom -> 12

parseWaveNum :: Int -> Maybe InstLfoWave
parseWaveNum = \case
  0 -> Just InstLfoTriangle
  3 -> Just InstLfoSquare
  6 -> Just InstLfoSaw
  7 -> Just InstLfoRevSaw
  12 -> Just InstLfoRandom
  _ -> Nothing

parseLoopType :: Text -> Maybe InstLoopType
parseLoopType = \case
  "forward" -> Just InstLoopTypeForward
  "backward" -> Just InstLoopTypeBackward
  "alternate" -> Just InstLoopTypeAlternate
  _ -> Nothing

parseLoopMode :: Text -> Maybe Bool
parseLoopMode = \case
  "loop_continuous" -> Just True
  "no_loop" -> Just False
  _ -> Nothing

{-
mapping:
lfo/eg01 -> volume
lfo/eg02 -> panning
lfo/eg03 -> cutoff
lfo/eg04 -> pitch

lfo attrs:
lfoXX_wave = wav type as int
lfoXX_freq = 0 to 20 in Hz
lfoXX_{cat}={depth}
(for pti translation need to convert Hz to steps:
24,16,12,8,6,4,3,2,3/2,1,3/4,1/2,3/8,1/3,....)

eg attrs:
egXX_attack = 0 to 100 float in seconds
egXX_decay = 0 to 100 float in seconds
egXX_sustain = 0 to 100 float in %
egXX_release = 0 to 100 float in seconds
egXX_{cat}={depth}

only ONE can be defined per category
if any attrs are defined, then lfo/eg_{cat}={depth} must also be defined
and all attrs must be defined with it
-}

data PatchErr = PatchErrWhatever
  deriving (Eq, Show)

instance Exception PatchErr

newtype SfzReader r a = SfzReader { unSfzReader :: ReaderT r (Except String) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader r)

runSfzReader :: SfzReader r a -> r -> Either String a
runSfzReader m = runExcept . runReaderT (unSfzReader m)

instance MonadFail (SfzReader r) where
  fail = SfzReader . throwError

askValM :: (SfzVal -> Maybe i) -> Text -> SfzReader SfzSection (Maybe i)
askValM f k = SfzReader (asks (\s -> Map.lookup k (ssAttrs s) >>= f))

guardValM :: Text -> Maybe i -> SfzReader SfzSection i
guardValM k mi =
  case mi of
    Nothing -> do
      n <- asks ssName
      fail ("Missing attr val for " ++ T.unpack n ++ " " ++ T.unpack k)
    Just i -> pure i

requireValM :: (SfzVal -> Maybe i) -> Text -> SfzReader SfzSection i
requireValM f k = do
  mi <- askValM f k
  guardValM k mi

defaultValM :: (SfzVal -> Maybe i) -> Text -> i -> SfzReader SfzSection i
defaultValM f k v = do
  mi <- askValM f k
  pure $! fromMaybe v mi

askSectionM :: Text -> SfzReader SfzSection a -> SfzReader SfzFile (Maybe a)
askSectionM n act = do
  mr <- asks $ \(SfzFile s) -> fmap (Seq.index s) (Seq.findIndexL (\t -> n == ssName t) s)
  case mr of
    Nothing -> pure Nothing
    Just r ->
      case runSfzReader act r of
        Left e -> fail e
        Right a -> pure (Just a)

guardSectionM :: Text -> Maybe a -> SfzReader r a
guardSectionM n ma =
  case ma of
    Nothing -> fail ("Missing section for " ++ T.unpack n)
    Just a -> pure a

requireSectionM :: Text -> SfzReader SfzSection a -> SfzReader SfzFile a
requireSectionM n act = askSectionM n act >>= guardSectionM n

onRegionsM :: SfzReader SfzSection a -> SfzReader SfzFile (Seq a)
onRegionsM act = do
  mayGlobalAttrs <- askSectionM "global" (asks ssAttrs)
  regionSections <- asks (\(SfzFile s) -> Seq.filter (\t -> "region" == ssName t) s)
  for regionSections $ \(SfzSection name regionAttrs) -> do
    let combinedAttrs = maybe regionAttrs (<> regionAttrs) mayGlobalAttrs
    case runSfzReader act (SfzSection name combinedAttrs) of
      Left e -> fail e
      Right a -> pure a

readDefPathM :: SfzReader SfzFile (Maybe FilePath)
readDefPathM =
  fmap join $ askSectionM "control" $ do
    askValM (fmap T.unpack . sfzValText) "default_path"

readParamsM :: SfzReader SfzFile InstParams
readParamsM =
  fmap (fromMaybe def) $ askSectionM "global" $ do
    pan <- defaultValM sfzValFloat "pan" 0
    let tune = undefined
        filt = undefined
        auto = undefined
    pure $! InstParams pan tune filt auto

forbidValM :: Text -> SfzReader SfzSection ()
forbidValM k = do
  mi <- askValM (const (Just ())) k
  unless (isNothing mi) (fail ("forbidden attribute present: " ++ T.unpack k))

readRegionM :: SfzReader SfzSection (InstRegion SfzSample)
readRegionM = do
  sampleStr <- requireValM sfzValText "sample"
  sample <- if
    | T.null sampleStr -> fail "empty sample string"
    | T.head sampleStr == '*' -> pure $! SfzSampleBuiltin (T.tail sampleStr)
    | otherwise -> pure $! SfzSampleFile (T.unpack sampleStr)
  keyRange <- InstKeyRange
    <$> requireValM sfzValInt "lokey"
    <*> requireValM sfzValInt "hikey"
    <*> requireValM sfzValInt "pitch_keycenter"
  loopModeStr <- defaultValM sfzValText "loop_mode" "no_loop"
  mayLoop <- case parseLoopMode loopModeStr of
    Just True -> fmap Just $ InstLoop
      <$> requireValM (sfzValText >=> parseLoopType) "loop_type"
      <*> requireValM sfzValInt "loop_start"
      <*> requireValM sfzValInt "loop_end"
    Just False -> Nothing <$ for_ ["loop_type", "loop_start", "loop_end"] forbidValM
    Nothing -> fail ("invalid loop_mode: " ++ T.unpack loopModeStr)
  pure $! InstRegion sample keyRange mayLoop Nothing

readRegionsM :: SfzReader SfzFile (Seq (InstRegion SfzSample))
readRegionsM = onRegionsM readRegionM

sfzToInst :: SfzFile -> Either String (Maybe FilePath, InstSpec SfzSample)
sfzToInst = runSfzReader $ do
  mayDefPath <- readDefPathM
  params <- readParamsM
  regions <- readRegionsM
  let inst = InstSpec params regions
  pure (mayDefPath, inst)

data SfzSample =
    SfzSampleFile !FilePath
  | SfzSampleBuiltin !Text
  deriving stock (Eq, Show)

instToSfz :: Maybe FilePath -> InstSpec SfzSample -> SfzFile
instToSfz mayDefPath (InstSpec {..}) =
  let controlS = SfzSection "control" (Map.fromList (join [fmap (("default_path",) . textSfzVal) (maybeToList mayDefPath)]))
      globalS = SfzSection "global" (instParamAttrs isParams)
      regionSS = fmap (SfzSection "region" . instRangeAttrs) isRegions
  in SfzFile (controlS :<| globalS :<| regionSS)

instParamAttrs :: InstParams -> SfzAttrs
instParamAttrs (InstParams {..}) = error "TODO"
  -- let x = 1
  -- in Map.fromList []

instRangeAttrs :: InstRegion SfzSample -> SfzAttrs
instRangeAttrs (InstRegion {..}) = error "TODO"
  -- let x = 1
  -- in Map.fromList []
