{-# LANGUAGE RecordWildCards #-}

module Scrapti.Convert
  ( loadAiff
  , loadWav
  , Neutral (..)
  , aiffToNeutral
  , wavToNeutral
  , loadNeutral
  , neutralToWav
  , neutralMono
  , neutralCrossFade
  , neutralCropLoop
  , neutralToSampleWav
  , readPtiWav
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless, (>=>))
import Dahdit (Int16LE, LiftedPrim, LiftedPrimArray (..), Seq (..), decodeFile)
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
import Scrapti.Aiff (Aiff, aiffGatherMarkers, aiffToPcmContainer)
import Scrapti.Common
  ( ConvertErr (..)
  , LoopMarkNames
  , LoopMarkPoints
  , LoopMarks (..)
  , SimpleMarker (..)
  , adjustMarker
  , findLoopMarks
  , recallLoopMarkNames
  , rethrow
  )
import Scrapti.Dsp
  ( Mod
  , PcmContainer (..)
  , PcmMeta (..)
  , SampleCount
  , applyMod
  , applyModGeneric
  , crop
  , ensureMonoFromLeft
  , linearCrossFade
  , reduceBitDepth
  )
import Scrapti.Wav
  ( Wav
  , WavChunk (..)
  , wavAddChunks
  , wavFromPcmContainer
  , wavGatherMarkers
  , wavToPcmContainer
  , wavUseLoopPoints
  , wavUseMarkers
  )
import System.FilePath (splitExtension)

convertMod :: (LiftedPrim a, LiftedPrim b) => Mod a b -> PcmContainer -> Either ConvertErr PcmContainer
convertMod modx con = either (Left . ConvertErrDsp) Right (applyMod modx con)

convertModGeneric :: (forall a. (LiftedPrim a, Integral a) => Mod a a) -> PcmContainer -> Either ConvertErr PcmContainer
convertModGeneric modx con = either (Left . ConvertErrDsp) Right (applyModGeneric modx con)

loadAiff :: FilePath -> IO Aiff
loadAiff = decodeFile >=> either throwIO pure . fst

loadWav :: FilePath -> IO Wav
loadWav = decodeFile >=> either throwIO pure . fst

data Neutral = Neutral
  { neCon :: !PcmContainer
  , neMarks :: !(Seq SimpleMarker)
  , neLoopMarks :: !(Maybe LoopMarkPoints)
  }
  deriving stock (Eq, Show)

guardSr :: Int -> Neutral -> IO ()
guardSr expectedSr ne = do
  let actualSr = pmSampleRate (pcMeta (neCon ne))
  unless (expectedSr == actualSr) (fail ("Expected SR: " ++ show expectedSr ++ " but got " ++ show actualSr))

-- NOTE: Taking sr as a param here so we don't have to interpret extended fp
aiffToNeutral :: Int -> Aiff -> Maybe LoopMarkNames -> Either ConvertErr Neutral
aiffToNeutral sr aiff mayNames = do
  neCon <- aiffToPcmContainer sr aiff
  let !neMarks = aiffGatherMarkers aiff
  neLoopMarks <- maybe (pure Nothing) (fmap Just . (`findLoopMarks` neMarks)) mayNames
  pure $! Neutral {..}

wavToNeutral :: Wav -> Maybe LoopMarkNames -> Either ConvertErr Neutral
wavToNeutral wav mayNames = do
  neCon <- wavToPcmContainer wav
  let !neMarks = wavGatherMarkers wav
  neLoopMarks <- maybe (pure Nothing) (fmap Just . (`findLoopMarks` neMarks)) mayNames
  pure $! Neutral {..}

loadNeutral :: Int -> Maybe LoopMarkNames -> FilePath -> IO Neutral
loadNeutral sr mayNames fp = do
  let (_, ext) = splitExtension fp
  ne <-
    if
        | ext == ".wav" -> do
            wav <- loadWav fp
            rethrow (wavToNeutral wav mayNames)
        | ext == ".aif" || ext == ".aifc" || ext == ".aiff" -> do
            aiff <- loadAiff fp
            rethrow (aiffToNeutral sr aiff mayNames)
        | otherwise -> fail ("Could not load with unknown extension: " ++ fp)
  guardSr sr ne
  pure ne

neutralToWav :: Int -> Neutral -> Wav
neutralToWav note (Neutral {..}) =
  let !sr = pmSampleRate (pcMeta neCon)
      !maySampleChunk = fmap (WavChunkSample . wavUseLoopPoints sr note) neLoopMarks
      !markChunks = if Seq.null neMarks then [] else let (!wcc, !wac) = wavUseMarkers neMarks in [WavChunkCue wcc, WavChunkAdtl wac]
      !chunks = Seq.fromList (markChunks ++ maybe [] pure maySampleChunk)
      !wav = wavFromPcmContainer neCon
  in  wavAddChunks chunks wav

neutralMono :: Neutral -> Either ConvertErr Neutral
neutralMono ne@(Neutral {..}) = do
  con' <- convertModGeneric ensureMonoFromLeft neCon
  pure $! ne {neCon = con'}

neutralDepth :: Neutral -> Either ConvertErr Neutral
neutralDepth ne@(Neutral {..}) = do
  case pmBitsPerSample (pcMeta neCon) of
    16 -> pure ne
    24 -> do
      con' <- convertMod reduceBitDepth neCon
      pure $! ne {neCon = con'}
    y -> Left (ConvertErrBadBps y)

neutralCrossFade :: SampleCount -> Neutral -> Either ConvertErr Neutral
neutralCrossFade width ne@(Neutral {..}) = do
  LoopMarks _ (_, !loopStart) (_, !loopEnd) _ <- maybe (Left ConvertErrNoLoopMarks) Right neLoopMarks
  let !loopStartPos = smPosition loopStart
      !loopEndPos = smPosition loopEnd
  con' <- convertModGeneric (linearCrossFade width (fromIntegral loopStartPos) (fromIntegral loopEndPos)) neCon
  pure $! ne {neCon = con'}

neutralCropLoop :: Neutral -> Either ConvertErr Neutral
neutralCropLoop (Neutral {..}) = do
  initLoopMarks <- maybe (Left ConvertErrNoLoopMarks) Right neLoopMarks
  let !names = recallLoopMarkNames initLoopMarks
      LoopMarks (_, !start) _ (_, !loopEnd) (_, !end) = initLoopMarks
      !startPos = smPosition start
      !loopEndPos = smPosition loopEnd
      !endPos = smPosition end
      !filteredMarks = Seq.filter (\sm -> let p = smPosition sm in p >= startPos && p <= loopEndPos) neMarks
      !withEndMarks = if endPos <= loopEndPos then filteredMarks else filteredMarks :|> end {smPosition = loopEndPos}
      !finalMarks = fmap (adjustMarker (-startPos)) withEndMarks
  !finalLoopMarks <- findLoopMarks names finalMarks
  con' <- convertModGeneric (crop (fromIntegral startPos) (fromIntegral loopEndPos)) neCon
  pure $! Neutral {neCon = con', neLoopMarks = Just finalLoopMarks, neMarks = finalMarks}

neutralIfHasMarks :: (Neutral -> Either e Neutral) -> Neutral -> Either e Neutral
neutralIfHasMarks f ne = do
  if isJust (neLoopMarks ne)
    then f ne
    else Right ne

neutralToSampleWav :: SampleCount -> Int -> Neutral -> Either ConvertErr Wav
neutralToSampleWav width note ne = fmap (neutralToWav note) (neutralMono ne >>= neutralDepth >>= neutralIfHasMarks (neutralCrossFade width) >>= neutralIfHasMarks neutralCropLoop)

readPtiWav :: Maybe LoopMarkNames -> FilePath -> IO (LiftedPrimArray Int16LE, Maybe LoopMarkPoints)
readPtiWav mayNames fp = do
  wav <- loadWav fp
  ne <- rethrow (wavToNeutral wav mayNames)
  let !meta = pcMeta (neCon ne)
  unless
    (pmNumChannels meta == 1 && pmSampleRate meta == 44100 && pmBitsPerSample meta == 16)
    (fail ("bad pti wav: " ++ fp))
  let !arr = LiftedPrimArray (pcData (neCon ne))
      !points = neLoopMarks ne
  pure (arr, points)
