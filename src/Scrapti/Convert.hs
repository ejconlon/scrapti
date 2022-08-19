{-# LANGUAGE RecordWildCards #-}

module Scrapti.Convert
  ( Neutral (..)
  , aiffToNeutral
  , neutralToWav
  , neutralToSampleWav
  ) where

import Dahdit (Int16LE, LiftedPrim, Seq)
import qualified Data.Sequence as Seq
import Scrapti.Aiff (Aiff, aiffGatherMarkers, aiffToPcmContainer)
import Scrapti.Common (ConvertErr (..), LoopMarkNames, LoopMarkPoints, SimpleMarker, findLoopMarks)
import Scrapti.Dsp (Mod, PcmContainer, applyMod, ensureMonoFromLeft, modAndThen, modId)
import Scrapti.Wav (Wav, WavChunk (..), wavAddChunks, wavFromPcmContainer, wavUseMarkers)

convertMod :: (LiftedPrim a, LiftedPrim b) => Mod a b -> PcmContainer -> Either ConvertErr PcmContainer
convertMod modx con = either (Left . ConvertErrDsp) Right (applyMod modx con)

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

neutralToSampleWav :: Neutral -> Either ConvertErr Wav
neutralToSampleWav ne@(Neutral {..}) = do
  let !fadeMod = modId
      !sampleMod = (ensureMonoFromLeft `modAndThen` fadeMod) :: Mod Int16LE Int16LE
  con' <- convertMod sampleMod neCon
  let !ne' = ne { neCon = con' }
  Right $! neutralToWav ne'
