{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Main
  ( main
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Dahdit (encodeFile)
import Data.Default (Default (..))
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (for)
import Options.Applicative
  ( Parser
  , ParserInfo
  , argument
  , command
  , execParser
  , fullDesc
  , header
  , helper
  , idm
  , info
  , progDesc
  , str
  , subparser
  , (<**>)
  )
import Scrapti.Common (defaultLoopMarkNames)
import Scrapti.Convert (Neutral (..), loadNeutral, neutralToSampleWav)
import Scrapti.Midi.Notes (LinNote (..), octToLin)
import Scrapti.Patches.ConvertPti (PtiPatch (PtiPatch), instToPtiPatches)
import Scrapti.Patches.ConvertSfz (SfzSample (..), instToSfz, sfzToInst)
import Scrapti.Patches.Inst (InstControl (..), InstDef (..))
import Scrapti.Patches.Loader (LoadedSample (..), Sample (sampleNote, samplePath), initializeInst, matchSamples)
import Scrapti.Patches.Sfz (findSfzSection, parseSfz, renderSfz, replaceSfzSection)
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , removeDirectoryRecursive
  )
import System.FilePath (takeBaseName, takeFileName, (<.>), (</>))

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists fp = do
  exists <- doesPathExist fp
  when exists (removeDirectoryRecursive fp)

data PackRef = PackRef !FilePath !String
  deriving stock (Eq, Show)

mkPackDir :: PackRef -> FilePath
mkPackDir (PackRef fp name) = fp </> name

parsePackRef :: Parser PackRef
parsePackRef = PackRef <$> argument str idm <*> argument str idm

canonPackRef :: PackRef -> IO (FilePath, String)
canonPackRef (PackRef fp name) = do
  fp' <- canonicalizePath fp
  assertDirExists fp'
  pure (fp' </> name, name)

data Action
  = ActionInit !PackRef
  | ActionFreeze !PackRef
  | ActionClean !PackRef
  deriving stock (Eq, Show)

run :: Action -> IO ()
run = \case
  ActionInit ref -> runInit ref
  ActionFreeze ref -> runFreeze ref
  ActionClean ref -> runClean ref

assertDirExists :: FilePath -> IO ()
assertDirExists fp = do
  exists <- doesDirectoryExist fp
  unless exists (fail ("Required directory does not exist: " ++ fp))

assertNotExists :: FilePath -> IO ()
assertNotExists fp = do
  exists <- doesPathExist fp
  when exists (fail ("Path already exists: " ++ fp))

runInit :: PackRef -> IO ()
runInit pr = do
  -- constants
  let sr = 44100
      xfadeWidth = 2250
      markNames = Just defaultLoopMarkNames
  (packDir, name) <- canonPackRef pr
  -- Resolve src and dest dirs
  let srcDir = packDir </> "sources"
      sampDir = packDir </> "samples"
      sfzFile = packDir </> name <.> "sfz"
  assertDirExists srcDir
  -- Remove existing sample dir
  removeDirectoryIfExists sampDir
  -- Find samples
  srcSamples <- matchSamples (T.pack name) "aif" srcDir
  when (Seq.null srcSamples) (fail "Found no samples")
  -- Convert them and write them out
  createDirectoryIfMissing True sampDir
  newSamples <- for srcSamples $ \srcSample -> do
    let srcFile = samplePath srcSample
        destFile = sampDir </> takeBaseName srcFile <.> "wav"
    putStrLn ("Processing: " ++ srcFile)
    sourceNe <- loadNeutral sr markNames srcFile
    let noteNum = unLinNote (octToLin (sampleNote srcSample))
    convertedWav <- either throwIO pure (neutralToSampleWav xfadeWidth noteNum sourceNe)
    encodeFile convertedWav destFile
    pure $! srcSample {samplePath = destFile}
  -- Initialize instrument
  sfzExists <- doesFileExist sfzFile
  mayGlob <-
    if sfzExists
      then do
        sfzContents <- TIO.readFile sfzFile
        sfzRep <- either fail pure (parseSfz sfzContents)
        pure $! findSfzSection "global" sfzRep
      else pure Nothing
  instSpec <- initializeInst sr markNames newSamples
  let instDef = InstDef (InstControl Nothing (Just "samples/")) (fmap (SfzSampleFile . takeFileName . lsPath) instSpec)
  sfzRep <- either fail pure (instToSfz instDef)
  sfzRep' <- maybe (pure sfzRep) (maybe (fail "missing global section") pure . replaceSfzSection "global" sfzRep) mayGlob
  let sfzContents = renderSfz sfzRep'
  TIO.writeFile sfzFile sfzContents

runFreeze :: PackRef -> IO ()
runFreeze pr = do
  -- constants
  let sr = 44100
      markNames = Just defaultLoopMarkNames
  (packDir, name) <- canonPackRef pr
  let sampDir = packDir </> "samples"
      instDir = packDir </> "instruments"
      sfzFile = packDir </> name <.> "sfz"
  -- read sfz file
  sfzContents <- TIO.readFile sfzFile
  sfzRep <- either fail pure (parseSfz sfzContents)
  -- convert to inst and load stuff
  instRead <- either fail pure (sfzToInst sfzRep)
  instWrite <- for (idSpec instRead) $ \case
    SfzSampleFile fp -> fmap neCon (loadNeutral sr markNames (sampDir </> fp))
    SfzSampleBuiltin txt -> fail ("Cannot use builtin samples (" ++ T.unpack txt ++ ")")
  -- emit pti patches
  patches <- either fail pure (instToPtiPatches (T.pack name) Nothing def instWrite)
  createDirectoryIfMissing True instDir
  for_ patches $ \(PtiPatch nm _ pti) -> do
    encodeFile pti (instDir </> T.unpack nm <.> "pti")

runClean :: PackRef -> IO ()
runClean pr = do
  (packDir, _) <- canonPackRef pr
  -- Resolve dirs
  let sampDir = packDir </> "samples"
      instDir = packDir </> "instruments"
  -- Make sure
  putStrLn ("Are you sure you want to clean pack " ++ packDir ++ " ? [no|yes] ")
  ans <- getLine
  case ans of
    "yes" -> do
      putStrLn "Cleaning..."
      removeDirectoryIfExists sampDir
      removeDirectoryIfExists instDir
      putStrLn "Done"
    _ -> putStrLn "Skipping"

parser :: Parser Action
parser =
  subparser
    ( command "init" (info (ActionInit <$> parsePackRef) idm)
        <> command "freeze" (info (ActionFreeze <$> parsePackRef) idm)
        <> command "clean" (info (ActionClean <$> parsePackRef) idm)
    )

parserInfo :: ParserInfo Action
parserInfo =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "scrapti"
        <> header "magic for audio"
    )

main :: IO ()
main = execParser parserInfo >>= run
