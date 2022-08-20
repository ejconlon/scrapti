{-# LANGUAGE RecordWildCards #-}

module Scrapti.Convert
  ( Neutral (..)
  , aiffToNeutral
  , neutralToWav
  , neutralMono
  , neutralCrossFade
  , neutralCropLoop
  , neutralToSampleWav
  ) where

import Dahdit (LiftedPrim, Seq)
import qualified Data.Sequence as Seq
import Scrapti.Aiff (Aiff, aiffGatherMarkers, aiffToPcmContainer)
import Scrapti.Common (ConvertErr (..), LoopMarkNames, LoopMarkPoints, LoopMarks (..), SimpleMarker (..),
                       adjustLoopPoints, adjustMarker, findLoopMarks)
import Scrapti.Dsp (Mod, PcmContainer, applyMod, applyModGeneric, crop, ensureMonoFromLeft, linearCrossFade)
import Scrapti.Wav (Wav, WavChunk (..), wavAddChunks, wavFromPcmContainer, wavUseMarkers)

convertMod :: (LiftedPrim a, LiftedPrim b) => Mod a b -> PcmContainer -> Either ConvertErr PcmContainer
convertMod modx con = either (Left . ConvertErrDsp) Right (applyMod modx con)

convertModGeneric :: (forall a. (LiftedPrim a, Integral a) => Mod a a) -> PcmContainer -> Either ConvertErr PcmContainer
convertModGeneric modx con = either (Left . ConvertErrDsp) Right (applyModGeneric modx con)

data Neutral = Neutral
  { neCon :: !PcmContainer
  , neMarks :: !(Seq SimpleMarker)
  , neLoopMarks :: !(Maybe LoopMarkPoints)
  } deriving stock (Eq, Show)

-- NOTE: Taking sr as a param here so we don't have to interpret extended fp
aiffToNeutral :: Int -> Aiff -> Maybe LoopMarkNames -> Either ConvertErr Neutral
aiffToNeutral sr aiff mayNames = do
  neCon <- aiffToPcmContainer sr aiff
  let !neMarks = aiffGatherMarkers aiff
  neLoopMarks <- maybe (pure Nothing) (fmap Just . (`findLoopMarks` neMarks)) mayNames
  pure $! Neutral { .. }

neutralToWav :: Neutral -> Wav
neutralToWav (Neutral {..}) =
  let !maySampleChunk = Nothing -- error "TODO"
      (!wcc, !wac) = wavUseMarkers neMarks
      !chunks = Seq.fromList ([WavChunkCue wcc, WavChunkAdtl wac] ++ maybe [] pure maySampleChunk)
      !wav = wavFromPcmContainer neCon
  in wavAddChunks chunks wav

neutralMono :: Neutral -> Either ConvertErr Neutral
neutralMono ne@(Neutral {..}) = do
  con' <- convertModGeneric ensureMonoFromLeft neCon
  pure $! ne { neCon = con' }

neutralCrossFade :: Int -> Neutral -> Either ConvertErr Neutral
neutralCrossFade width ne@(Neutral {..}) = do
  LoopMarks _ (_, !loopStart) (_, !loopEnd) _ <- maybe (Left ConvertErrNoLoopMarks) Right neLoopMarks
  let !loopStartPos = smPosition loopStart
      !loopEndPos = smPosition loopEnd
  con' <- convertModGeneric (linearCrossFade width (fromIntegral loopStartPos) (fromIntegral loopEndPos)) neCon
  pure $! ne { neCon = con' }

neutralCropLoop :: Neutral -> Either ConvertErr Neutral
neutralCropLoop (Neutral {..}) = do
  -- TODO need to adjust marks and loop markers for the crop
  initLoopMarks <- maybe (Left ConvertErrNoLoopMarks) Right neLoopMarks
  let LoopMarks (_, !start) _ (_, !loopEnd) _ = initLoopMarks
      !startPos = smPosition start
      !loopEndPos = smPosition loopEnd
      !adjLoopMarks = adjustLoopPoints (-startPos) initLoopMarks
      !finalLoopMarks = adjLoopMarks { lmEnd = lmLoopEnd adjLoopMarks }
      !filteredMarks = Seq.filter (\sm -> let p = smPosition sm in p >= startPos && p <= loopEndPos) neMarks
      !finalMarks = fmap (adjustMarker (-startPos)) filteredMarks
  con' <- convertModGeneric (crop (fromIntegral startPos) (fromIntegral loopEndPos)) neCon
  pure $! Neutral { neCon = con', neLoopMarks = Just finalLoopMarks, neMarks = finalMarks }

neutralToSampleWav :: Int -> Neutral -> Either ConvertErr Wav
neutralToSampleWav width ne = fmap neutralToWav (neutralMono ne >>= neutralCrossFade width >>= neutralCropLoop)
