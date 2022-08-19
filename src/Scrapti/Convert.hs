module Scrapti.Convert where

import Dahdit (Int16LE)
import qualified Data.Sequence as Seq
import Scrapti.Aiff (Aiff, aiffGatherMarkers, aiffToPcmContainer)
import Scrapti.Dsp (DspErr, applyMod, ensureMonoFromLeft)
import Scrapti.Wav (Wav, WavChunk (..), wavAddChunks, wavFromPcmContainer, wavUseMarkers)

convertAiff :: Aiff -> Either DspErr Wav
convertAiff aiff = do
  let con = aiffToPcmContainer aiff
      marks = aiffGatherMarkers aiff
  con' <- applyMod @Int16LE @Int16LE ensureMonoFromLeft con
  let (wcc, wac) = wavUseMarkers marks
      chunks = Seq.fromList [WavChunkCue wcc, WavChunkAdtl wac]
      wav = wavFromPcmContainer con'
      wav' = wavAddChunks chunks wav
  pure wav'
