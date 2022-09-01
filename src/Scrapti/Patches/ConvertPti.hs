{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Patches.ConvertPti
  ( PtiSample
  , PtiPatch (..)
  , instToPtiPatches
  ) where

import Dahdit (Int16LE, LiftedPrimArray, BoolByte (..), StaticBytes (..), sizeofLiftedPrimArray, Word16LE (..), FloatLE (..))
import Scrapti.Tracker.Pti (Pti, mkPti, Header (..), Preamble (..), Block (..), SamplePlayback (..), Auto (..), Filter (..), Lfo (..), InstParams (..), FilterType (..), AutoType (..), AutoEnvelope (..), LfoType (..), LfoSteps (..))
import Scrapti.Patches.Inst (InstSpec (..), InstKeyRange (..), InstConfig (..), InstRegion (..), InstCrop (..), InstLoop (..), InstLoopType (..), InstBlock (..), InstFilter (..), InstAuto (..), InstFilterType (..), InstEnv (..), InstLfo (..), InstLfoWave (..))
import Data.Sequence (Seq)
import Data.Traversable (for)
import Data.Default (Default(..))
import Data.Text (Text)
import Scrapti.Midi.Notes (NotePref(..), renderNote, LinNote (..), linToOct, linSubInterval, Interval (..))
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Short as BSS
import Control.Monad (unless)
import Data.Ratio ((%))
import Data.Foldable (minimumBy)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (\x y -> compare (f x) (f y))

findClosest :: Rational -> [(a, Rational)] -> a
findClosest x ps = fst (minimumOn (\(_, y) -> abs (x - y)) ps)

-- 0 <= x / y <= 1
shortRatioPercent :: (Real a, Real b) => a -> b -> Word16LE
shortRatioPercent x y =
  let x' = toRational x
      y' = toRational y
      r = (x' * 65535) / y'
  in round r

clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal = max maxVal . min minVal

-- Tempo in BPM
newtype Tempo = Tempo { unTempo :: Rational }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Default Tempo where
  def = Tempo 120

-- -- tempo = 120 bpm how long does a beat take? in seconds per beat 1 minute (60 s) a 120 beats/min
-- -- is 60 s/min / 120 beats/min
-- beatPeriod :: Tempo -> Rational
-- beatPeriod (Tempo t) = 60 / t

type PtiSample = LiftedPrimArray Int16LE

data PtiPatch = PtiPatch
  { ppKeyRange :: !InstKeyRange
  , ppPti :: !Pti
  } deriving stock (Eq, Show)

convertPreamble :: Text -> LinNote -> InstRegion PtiSample -> Preamble
convertPreamble namePrefix linNote (InstRegion samp _ mayLoop mayCrop) =
  let prettyNote = renderNote NotePrefFlat (linToOct linNote)
      name = namePrefix <> "-" <> prettyNote
      sampLen = sizeofLiftedPrimArray samp
      sampPlay = case mayLoop of
        Nothing -> SPOneShot
        Just (InstLoop ty _ _) -> case ty of
          InstLoopTypeForward -> SPForwardLoop
          InstLoopTypeBackward -> SPBackwardLoop
          InstLoopTypeAlternate -> SPPingPongLoop
      playStart = case mayCrop of
        Just (InstCrop start _) -> shortRatioPercent start sampLen
        Nothing -> 0
      loopStartRaw = case mayLoop of
        Just (InstLoop _ start _) -> shortRatioPercent start sampLen
        Nothing -> 0
      loopStart = if loopStartRaw > playStart then loopStartRaw else playStart + 1
      playEnd = case mayCrop of
        Just (InstCrop _ end) -> shortRatioPercent end sampLen
        Nothing -> 0
      loopEndRaw = case mayLoop of
        Just (InstLoop _ _ end) -> shortRatioPercent end sampLen
        Nothing -> 0
      loopEnd = if loopEndRaw < playEnd then loopEndRaw else playEnd - 1
  in def
    { preIsWavetable = BoolByte False
    , preName = StaticBytes (BSS.toShort (TE.encodeUtf8 name))
    , preSampleLength = fromIntegral sampLen
    , preSamplePlayback = sampPlay
    , prePlaybackStart = playStart
    , preLoopStart = loopStart
    , preLoopEnd = loopEnd
    , prePlaybackEnd = playEnd
    }

convertFilter :: Maybe InstFilter -> Either String Filter
convertFilter = \case
  Nothing -> Right def
  Just (InstFilter ty cut res) ->
    let ty' = case ty of
          InstFilterTypeLowpass -> FTLowPass
          InstFilterTypeHighpass -> FTHighPass
          InstFilterTypeBandpass -> FTBandPass
        -- TODO appropriately scale cut and res
        cut' = FloatLE (fromRational cut)
        res' = FloatLE (fromRational res)
    in Right (Filter cut' res' ty')

convertBlock :: (Applicative m, Default b) => (a -> m b) -> InstBlock a -> m (Block b)
convertBlock f b = fmap (\(InstBlock vol' pan' cut' fine') -> def { blockVolume = vol', blockPanning = pan' , blockCutoff = cut', blockFinetune = fine' }) (traverse f b)

splitBlock :: Block (a, b) -> (Block a, Block b)
splitBlock (Block (a0, b0) (a1, b1) (a2, b2) (a3, b3) (a4, b4) (a5, b5)) =
  (Block a0 a1 a2 a3 a4 a5, Block b0 b1 b2 b3 b4 b5)

convertSplitBlock :: (Applicative m, Default b, Default c) => (a -> m (b, c)) -> InstBlock a -> m (Block b, Block c)
convertSplitBlock f = fmap splitBlock . convertBlock f

-- Calculate cycles/step for each setting
stepCount :: LfoSteps -> Rational
stepCount = \case
  LS24 -> 24
  LS16 -> 16
  LS12 -> 12
  LS8 -> 8
  LS6 -> 6
  LS4 -> 4
  LS3 -> 3
  LS2 -> 2
  LS3Over2 -> 3 % 2
  LS1 -> 1
  LS3Over4 -> 3 % 4
  LS1Over2 -> 1 % 2
  LS3Over8 -> 3 % 8
  LS1Over3 -> 1 % 3
  LS1Over4 -> 1 % 4
  LS3Over16 -> 3 % 16
  LS1Over6 -> 1 % 6
  LS1Over8 -> 1 % 8
  LS1Over12 -> 1 % 12
  LS1Over16 -> 1 % 16
  LS1Over24 -> 1 % 24
  LS1Over32 -> 1 % 32
  LS1Over48 -> 1 % 48
  LS1Over64 -> 1 % 64

stepAssoc :: [(LfoSteps, Rational)]
stepAssoc = fmap (\s -> (s, stepCount s)) [minBound .. maxBound]

closestStep :: Rational -> LfoSteps
closestStep = flip findClosest stepAssoc

-- tempo is in steps/min, freq is in cycles/min, freq/tempo is cycles/step
convertSteps :: Tempo -> Rational -> LfoSteps
convertSteps tempo freq = closestStep (freq / unTempo tempo)

convertAuto :: Tempo -> InstBlock (Maybe InstAuto) -> Either String (Block Auto, Block Lfo)
convertAuto tempo = convertSplitBlock $ \case
  Nothing -> Right (def { autoType = ATOff }, def)
  Just instAuto ->
    case instAuto of
      InstAutoEnv instEnv ->
        -- TODO appropriately scale env
        let env = def
              { aeAmount = fromRational (ieDepth instEnv)
              , aeAttack = clamp 0 10000 (round (1000 * ieAttack instEnv))
              , aeDecay = clamp 0 10000 (round (1000 * ieDecay instEnv))
              , aeSustain = fromRational (ieSustain instEnv)
              , aeRelease = clamp 0 10000 (round (1000 * ieRelease instEnv))
              }
            ae = def { autoType = ATEnvelope, autoEnvelope = env }
        in Right (ae, def)
      InstAutoLfo instLfo ->
        -- TODO appropriately scale lfo
        let ty = case ilWave instLfo of
              InstLfoRevSaw -> LTRevSaw
              InstLfoSaw -> LTSaw
              InstLfoTriangle -> LTTriangle
              InstLfoSquare -> LTSquare
              InstLfoRandom -> LTRandom
            lfo = def
              { lfoType = ty
              , lfoSteps = convertSteps tempo (ilFreq instLfo)
              , lfoAmount = fromRational (ilDepth instLfo)
              }
        in Right (def { autoType = ATLfo }, lfo)

-- interval - negative difference from center note (possibly zero)
-- tune - ratio of semitones
-- panning - left to right (TODO min/max +-2000? look at sfz docs)
convertParams :: Interval -> Rational -> Rational -> Either String InstParams
convertParams interval _panning tune = do
  let (coarseRaw, fine) = properFraction tune
  let coarse = Interval coarseRaw + interval
  unless (coarse >= -24 && coarse <= 24) (Left ("tune out of range: interval " ++ show (unInterval interval) ++ " " ++ show tune))
  let tuneArg = fromIntegral (unInterval coarse)
      fineTuneArg = clamp (-100) 100 (round (fine * 100))
      panningArg = 0 -- TODO convert panning
  pure $! def
    { ipTune = tuneArg
    , ipFineTune = fineTuneArg
    , ipPanning = panningArg
    }

instRegionToPti :: Text -> Maybe LinNote -> Tempo -> InstConfig -> InstRegion PtiSample -> Either String Pti
instRegionToPti namePrefix mayCenterNote tempo instConfig instRegion = do
  let linNote = LinNote (fromInteger (ikrSampKey (irKeyRange instRegion)))
      interval = maybe 0 (linSubInterval linNote) mayCenterNote
  let preamble = convertPreamble namePrefix linNote instRegion
  (autoBlock, lfoBlock) <- convertAuto tempo (icAuto instConfig)
  filt <- convertFilter (icFilter instConfig)
  ptiParams <- convertParams interval (icPanning instConfig) (icTune instConfig)
  let header = Header preamble autoBlock lfoBlock filt ptiParams def def def
  let samp = irSample instRegion
  pure $! mkPti header samp

instToPtiPatches :: Text -> Maybe LinNote -> Tempo -> InstSpec PtiSample -> Either String (Seq PtiPatch)
instToPtiPatches namePrefix mayCenterNote tempo (InstSpec instParams regions) = do
  for regions $ \region -> do
    let kr = irKeyRange region
    pti <- instRegionToPti namePrefix mayCenterNote tempo instParams region
    pure $! PtiPatch kr pti
