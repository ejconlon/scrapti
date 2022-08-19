{-# LANGUAGE RecordWildCards #-}

module Scrapti.Convert
  ( Neutral (..)
  , aiffToNeutral
  , neutralToWav
  , neutralMono
  , neutralCrossFade
  ) where

import Dahdit (LiftedPrim, Seq)
import qualified Data.Sequence as Seq
import Scrapti.Aiff (Aiff, aiffGatherMarkers, aiffToPcmContainer)
import Scrapti.Common (ConvertErr (..), LoopMarkNames, LoopMarkPoints, LoopMarks (..), SimpleMarker (..), findLoopMarks)
import Scrapti.Dsp (Mod, PcmContainer, applyMod, applyModGeneric, ensureMonoFromLeft, linearCrossFade)
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

fadeMod :: Int -> SimpleMarker -> SimpleMarker -> forall a. (LiftedPrim a, Integral a) => Mod a a
fadeMod width loopStart loopEnd = linearCrossFade width (fromIntegral (smPosition loopStart)) (fromIntegral (smPosition loopEnd))

neutralCrossFade :: Int -> Neutral -> Either ConvertErr Neutral
neutralCrossFade width ne@(Neutral {..}) = do
  LoopMarks _ (_, loopStart) (_, loopEnd) _ <- maybe (Left ConvertErrNoLoopMarks) Right neLoopMarks
  con' <- convertModGeneric (fadeMod width loopStart loopEnd) neCon
  pure $! ne { neCon = con' }
