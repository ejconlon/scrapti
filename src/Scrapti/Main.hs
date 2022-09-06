{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Main
  ( main
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Dahdit (put, runPutFile)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (for)
import Options.Applicative
import Scrapti.Common
import Scrapti.Convert (loadNeutral, neutralToSampleWav)
import Scrapti.Midi.Notes (LinNote (..), octToLin)
import Scrapti.Patches.ConvertSfz (SfzSample (..), instToSfz)
import Scrapti.Patches.Inst (InstControl (..), InstDef (..))
import Scrapti.Patches.Loader (LoadedSample (..), Sample (sampleNote, samplePath), initializeInst, matchSamples)
import Scrapti.Patches.Sfz (renderSfz)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist,
                         removeDirectoryRecursive)
import System.FilePath ((<.>), (</>))

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

data Action =
    ActionInit !PackRef
  | ActionClean !PackRef
  deriving stock (Eq, Show)

run :: Action -> IO ()
run = \case
  ActionInit ref -> runInit ref
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
        destFile = srcFile <.> "wav"
    sourceNe <- loadNeutral sr markNames srcFile
    let noteNum = unLinNote (octToLin (sampleNote srcSample))
    convertedWav <- either throwIO pure (neutralToSampleWav noteNum xfadeWidth sourceNe)
    runPutFile destFile (put convertedWav)
    pure $! srcSample { samplePath = destFile }
  -- Initialize instrument
  sfzExists <- doesFileExist sfzFile
  unless sfzExists $ do
    instSpec <- initializeInst sr markNames newSamples
    let instDef = InstDef (InstControl Nothing (Just "samples")) (fmap (SfzSampleFile . lsPath) instSpec)
    sfzRep <- either fail pure (instToSfz instDef)
    let sfzContents = renderSfz sfzRep
    TIO.writeFile sfzFile sfzContents

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists fp = do
  exists <- doesPathExist fp
  when exists (removeDirectoryRecursive fp)

runClean :: PackRef -> IO ()
runClean pr = do
  (packDir, _) <- canonPackRef pr
  -- Resolve dirs
  let sampDir = packDir </> "samples"
  -- Make sure
  putStrLn ("Are you sure you want to clean pack " ++ packDir ++ " ? [no|yes] ")
  ans <- getLine
  case ans of
    "yes" -> do
      putStrLn "Cleaning..."
      removeDirectoryIfExists sampDir
      putStrLn "Done"
    _ -> putStrLn "Skipping"

parser :: Parser Action
parser = subparser
  (  command "init" (info (ActionInit <$> parsePackRef) idm)
  <> command "clean" (info (ActionClean <$> parsePackRef) idm)
  )

parserInfo :: ParserInfo Action
parserInfo = info (parser <**> helper)
  (  fullDesc
  <> progDesc "scrapti"
  <> header "magic for audio"
  )

main :: IO ()
main = do
  a <- execParser parserInfo
  run a
