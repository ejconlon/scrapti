{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scrapti.Patches.ConvertSfz
  ( PatchErr (..)
  , sfzToInst
  , SfzSample (..)
  , instToSfz
  ) where

import Control.Exception (Exception)
import Control.Monad (join, unless, (>=>))
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT, execWriterT)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Default (Default (..))
import Data.Foldable (for_, traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Scrapti.Patches.Inst (InstAuto (..), InstAutoTarget (..), InstAutoType (..), InstBlock (..), InstConfig (..),
                             InstControl (..), InstCrop (..), InstDef (..), InstEnv (..), InstFilter (..),
                             InstFilterType (..), InstKeyRange (..), InstLfo (..), InstLfoWave (..), InstLoop (..),
                             InstLoopType (..), InstRegion (..), InstSpec (..), Tempo (..), traverseBlock_)
import Scrapti.Patches.Sfz (SfzAttrs, SfzFile (..), SfzSection (..), SfzVal (..), sfzValFloat, sfzValInt, sfzValText,
                            textSfzVal)

renderWaveNum :: InstLfoWave -> Integer
renderWaveNum = \case
  InstLfoRevSaw -> 7
  InstLfoSaw -> 6
  InstLfoTriangle -> 0
  InstLfoSquare -> 3
  InstLfoRandom -> 12

parseWaveNum :: Integer -> Maybe InstLfoWave
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

renderLoopType :: InstLoopType -> Text
renderLoopType = \case
  InstLoopTypeForward -> "forward"
  InstLoopTypeBackward -> "backward"
  InstLoopTypeAlternate -> "alternate"

data LoopMode = LoopModeOn | LoopModeOff
  deriving stock (Eq, Show)

parseLoopMode :: Text -> Maybe LoopMode
parseLoopMode = \case
  "loop_continuous" -> Just LoopModeOn
  "no_loop" -> Just LoopModeOff
  _ -> Nothing

parseFilType :: Text -> Maybe InstFilterType
parseFilType = \case
  "lpf_2p" -> Just InstFilterTypeLowpass
  "hpf_2p" -> Just InstFilterTypeHighpass
  "bpf_2p" -> Just InstFilterTypeBandpass
  _ -> Nothing

renderFilType :: InstFilterType -> Text
renderFilType = \case
  InstFilterTypeLowpass -> "lpf_2p"
  InstFilterTypeHighpass -> "hpf_2p"
  InstFilterTypeBandpass -> "bpf_2p"

data SfzSample =
    SfzSampleFile !FilePath
  | SfzSampleBuiltin !Text
  deriving stock (Eq, Show)

instance ToJSON SfzSample where
  toJSON = toJSON . renderSfzSample

instance FromJSON SfzSample where
  parseJSON = \case
    String txt ->
      if
        | T.null txt -> fail "Empty SfzSample"
        | T.head txt == '*' -> pure (SfzSampleBuiltin (T.tail txt))
        | otherwise -> pure (SfzSampleFile (T.unpack txt))
    _ -> fail "Non-string SfzSample"

renderSfzSample :: SfzSample -> Text
renderSfzSample = \case
  SfzSampleFile fp -> T.pack fp
  SfzSampleBuiltin b -> T.cons '*' b

renderAutoTarget :: InstAutoTarget -> (Text, Text)
renderAutoTarget = \case
  InstAutoTargetVolume -> ("01", "volume")
  InstAutoTargetPanning -> ("02", "panning")
  InstAutoTargetCutoff -> ("03", "cutoff")
  InstAutoTargetFinetune -> ("04", "finetune")

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

readControlM :: SfzReader SfzFile InstControl
readControlM = do
  fmap (fromMaybe def) $ askSectionM "control" $
    InstControl
      <$> askValM (fmap Tempo . sfzValFloat) "hint_tempo"
      <*> askValM (fmap T.unpack . sfzValText) "default_path"

data AutoChoice = AutoChoiceLfo | AutoChoiceEnv
  deriving stock (Eq, Show)

readMayLfoM :: (Text -> Text) -> Maybe Rational -> SfzReader SfzSection (Maybe InstLfo)
readMayLfoM f mayDepth = do
  mayWave <- askValM (sfzValInt >=> parseWaveNum) (f "wave")
  mayFreq <- askValM sfzValFloat (f "freq")
  pure $! InstLfo <$> mayWave <*> mayFreq <*> mayDepth

readLfoM :: (Text -> Text) -> Rational -> SfzReader SfzSection InstLfo
readLfoM f depth = do
  wave <- requireValM (sfzValInt >=> parseWaveNum) (f "wave")
  freq <- requireValM sfzValFloat (f "freq")
  pure $! InstLfo wave freq depth

readMayEnvM :: (Text -> Text) -> Maybe Rational -> SfzReader SfzSection (Maybe InstEnv)
readMayEnvM f mayDepth = do
  mayAttack <- askValM sfzValFloat (f "attack")
  mayDecay <- askValM sfzValFloat (f "decay")
  maySustain <- askValM sfzValFloat (f "sustain")
  mayRelease <- askValM sfzValFloat (f "release")
  pure $! InstEnv <$> mayAttack <*> mayDecay <*> maySustain <*> mayRelease <*> mayDepth

readEnvM :: (Text -> Text) -> Rational -> SfzReader SfzSection InstEnv
readEnvM f depth = do
  attack <- requireValM sfzValFloat (f "attack")
  decay <- requireValM sfzValFloat (f "decay")
  sustain <- requireValM sfzValFloat (f "sustain")
  release <- requireValM sfzValFloat (f "release")
  pure $! InstEnv attack decay sustain release depth

readAutoM :: Text -> Text -> SfzReader SfzSection InstAuto
readAutoM idx cat = do
  let mkKey ty key = ty <> idx <> "_" <> key
  mayLfoDepth <- askValM sfzValFloat (mkKey "lfo" cat)
  mayEnvDepth <- askValM sfzValFloat (mkKey "env" cat)
  ty <- case (mayLfoDepth, mayEnvDepth) of
    (Nothing, Nothing) -> pure InstAutoTypeOff
    (Nothing, Just _) -> pure InstAutoTypeEnv
    (Just _, Nothing) -> pure InstAutoTypeLfo
    (Just lfoDepth, Just envDepth) ->
      if
        | lfoDepth == 0 && envDepth == 0 -> pure InstAutoTypeOff
        | lfoDepth == 0 && envDepth /= 0 -> pure InstAutoTypeEnv
        | lfoDepth /= 0 && envDepth == 0 -> pure InstAutoTypeLfo
        | otherwise -> fail ("Forbidden lfo and env for category " ++ T.unpack cat)
  mayEnv <- maybe (readMayEnvM (mkKey "env") mayEnvDepth) (fmap Just . readEnvM (mkKey "env")) mayEnvDepth
  mayLfo <- maybe (readMayLfoM (mkKey "lfo") mayLfoDepth) (fmap Just . readLfoM (mkKey "lfo")) mayLfoDepth
  pure $! InstAuto ty mayEnv mayLfo

readAutoBlockM :: SfzReader SfzSection (InstBlock InstAuto)
readAutoBlockM = InstBlock
  <$> readAutoM "01" "volume"
  <*> readAutoM "02" "panning"
  <*> readAutoM "03" "cutoff"
  <*> readAutoM "04" "pitch"

readFiltM :: SfzReader SfzSection (Maybe InstFilter)
readFiltM = do
  mayFilTypeStr <- askValM sfzValText "fil_type"
  case mayFilTypeStr of
    Nothing -> Nothing <$ for_ ["cutoff", "resonance"] forbidValM
    Just filTypeStr -> case parseFilType filTypeStr of
      Just filType -> fmap Just $ InstFilter filType
        <$> requireValM sfzValFloat "cutoff"
        <*> defaultValM sfzValFloat "resonance" 0
      Nothing -> fail ("Invalid filter type: " ++ T.unpack filTypeStr)

readParamsM :: SfzReader SfzFile InstConfig
readParamsM =
  fmap (fromMaybe def) $ askSectionM "global" $ do
    pan <- defaultValM sfzValFloat "pan" 0
    tune <- defaultValM sfzValFloat "tune" 0
    filt <- readFiltM
    auto <- readAutoBlockM
    pure $! InstConfig pan tune filt auto

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
    <*> requireValM sfzValInt "pitch_keycenter"
    <*> requireValM sfzValInt "hikey"
  loopModeStr <- defaultValM sfzValText "loop_mode" "no_loop"
  mayLoop <- case parseLoopMode loopModeStr of
    Just LoopModeOn -> fmap Just $ InstLoop
      <$> defaultValM (sfzValText >=> parseLoopType) "loop_type" InstLoopTypeForward
      <*> defaultValM sfzValInt "loop_start" 0
      <*> requireValM sfzValInt "loop_end"
    Just LoopModeOff -> Nothing <$ for_ ["loop_type", "loop_start", "loop_end"] forbidValM
    Nothing -> fail ("invalid loop_mode: " ++ T.unpack loopModeStr)
  mayCrop <- fmap (fmap (InstCrop 0)) (askValM sfzValInt "end")
  pure $! InstRegion sample keyRange mayLoop mayCrop

readRegionsM :: SfzReader SfzFile (Seq (InstRegion SfzSample))
readRegionsM = onRegionsM readRegionM

sfzToInst :: SfzFile -> Either String (InstDef SfzSample)
sfzToInst = runSfzReader $ do
  control <- readControlM
  params <- readParamsM
  regions <- readRegionsM
  let inst = InstSpec params regions
  pure $! InstDef control inst

newtype SfzWriter w a = SfzWriter { unSfzWriter :: WriterT w (Except String) a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter w)

instance Monoid w => MonadFail (SfzWriter w) where
  fail = SfzWriter . throwError

execSfzWriter :: SfzWriter w () -> Either String w
execSfzWriter = runExcept . execWriterT . unSfzWriter

writeFiltM :: InstFilter -> SfzWriter SfzAttrs ()
writeFiltM (InstFilter {..}) =
  tell $ Map.fromList
    [ ("fil_type", SfzValText (renderFilType ifType))
    , ("cutoff", SfzValFloat ifCutoff)
    , ("resonance", SfzValFloat ifResonance)
    ]

writeAutoEnvM :: (Text -> Text) -> Text -> InstEnv -> SfzWriter SfzAttrs ()
writeAutoEnvM mkKey cat (InstEnv {..}) =
  tell $ Map.fromList
    [ (mkKey cat, SfzValFloat ieDepth)
    , (mkKey "attack", SfzValFloat ieAttack)
    , (mkKey "decay", SfzValFloat ieDecay)
    , (mkKey "sustain", SfzValFloat ieSustain)
    , (mkKey "release", SfzValFloat ieRelease)
    ]

writeAutoLfoM :: (Text -> Text) -> Text -> InstLfo -> SfzWriter SfzAttrs ()
writeAutoLfoM mkKey cat (InstLfo {..}) =
  tell $ Map.fromList
    [ (mkKey cat, SfzValFloat ilDepth)
    , (mkKey "wave", SfzValInt (renderWaveNum ilWave))
    , (mkKey "freq", SfzValFloat ilFreq)
    ]

writeAutoM :: InstAutoTarget -> InstAuto -> SfzWriter SfzAttrs ()
writeAutoM target (InstAuto _ env lfo) = do
  let (idx, cat) = renderAutoTarget target
      mkKey x key = x <> idx <> "_" <> key
  traverse_ (writeAutoEnvM (mkKey "env") cat) env
  traverse_ (writeAutoLfoM (mkKey "lfo") cat) lfo

instConfigM :: InstConfig -> SfzWriter SfzAttrs ()
instConfigM (InstConfig {..}) = do
  tell $ Map.fromList
    [ ("pan", SfzValFloat icPanning)
    , ("tune", SfzValFloat icTune)
    ]
  for_ icFilter writeFiltM
  traverseBlock_ writeAutoM icAuto

instRegionM :: InstRegion SfzSample -> SfzWriter SfzAttrs ()
instRegionM (InstRegion {..}) = do
  let InstKeyRange {..} = irKeyRange
  tell $ Map.fromList
    [ ("sample", SfzValText (renderSfzSample irSample))
    , ("lokey", SfzValInt ikrLowkey)
    , ("pitch_keycenter", SfzValInt ikrSampKey)
    , ("hikey", SfzValInt ikrHighkey)
    ]
  for_ irLoop $ \(InstLoop {..}) ->
    tell $ Map.fromList
      [ ("loop_mode", SfzValText "loop_continuous")
      , ("loop_type", SfzValText (renderLoopType ilType))
      , ("loop_start", SfzValInt ilLoopStart)
      , ("loop_end", SfzValInt ilLoopEnd)
      ]
  -- No way to specify crop points...
  for_ irCrop $ \(InstCrop {..}) -> do
    unless (icStart == 0) (fail "Can only crop end (start not supported)")
    tell $ Map.singleton "end" (SfzValInt icEnd)

instToSfz :: InstDef SfzSample -> Either String SfzFile
instToSfz (InstDef (InstControl mayTempo mayDefPath) (InstSpec {..})) = do
  let controlS = SfzSection "control" $ Map.fromList $ join
        [ fmap (("default_path",) . textSfzVal) (maybeToList mayDefPath)
        , fmap (("hint_tempo",) . SfzValFloat . unTempo) (maybeToList mayTempo)
        ]
  globalS <- fmap (SfzSection "global") (execSfzWriter (instConfigM isConfig))
  regionSS <- traverse (fmap (SfzSection "region") . execSfzWriter . instRegionM) isRegions
  pure $! SfzFile (controlS :<| globalS :<| regionSS)
