{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Convert
  ( convertAiff
  ) where

import Dahdit (Int16LE, LiftedPrim)
import qualified Data.Sequence as Seq
import Scrapti.Aiff (Aiff, aiffGatherMarkers, aiffToPcmContainer)
import Scrapti.Common (ConvertErr (..), LoopMarkNames, findLoopMarks)
import Scrapti.Dsp (Mod, PcmContainer, applyMod, ensureMonoFromLeft)
import Scrapti.Wav (Wav, WavChunk (..), wavAddChunks, wavFromPcmContainer, wavUseMarkers)

convertMod :: (LiftedPrim a, LiftedPrim b) => Mod a b -> PcmContainer -> Either ConvertErr PcmContainer
convertMod modx con = either (Left . ConvertErrDsp) Right (applyMod modx con)

-- NOTE: Taking sr as a param here so we don't have to interpret extended fp
convertAiff :: Int -> Aiff -> Maybe LoopMarkNames -> Either ConvertErr Wav
convertAiff sr aiff mayNames = do
  con <- aiffToPcmContainer sr aiff
  let marks = aiffGatherMarkers aiff
  mayPair <- flip (maybe (pure Nothing)) mayNames $ \names -> do
    loopMarks <- findLoopMarks names marks
    sampleChunk <- error "TODO"
    pure (Just (loopMarks, sampleChunk))
  con' <- convertMod @Int16LE @Int16LE ensureMonoFromLeft con
  -- TODO optionally crossfade and crop around loop markers
  let (wcc, wac) = wavUseMarkers marks
      chunks = Seq.fromList ([WavChunkCue wcc, WavChunkAdtl wac] ++ maybe [] (pure . snd) mayPair)
      wav = wavFromPcmContainer con'
      wav' = wavAddChunks chunks wav
  pure wav'
