{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Main
  ( main
  ) where

import Control.Monad (unless, when)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Options.Applicative
import Scrapti.Patches.Loader (matchSamples)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesPathExist,
                         removeDirectoryRecursive)
import System.FilePath ((</>))

data PackRef = PackRef !FilePath !String
  deriving stock (Eq, Show)

parsePackRef :: Parser PackRef
parsePackRef = PackRef <$> argument str idm <*> argument str idm

canonPackRef :: PackRef -> IO PackRef
canonPackRef (PackRef fp name) = do
  fp' <- canonicalizePath fp
  assertDirExists fp'
  pure $! PackRef fp name

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
  PackRef pack name <- canonPackRef pr
  -- Resolve src and dest dirs
  let srcDir = pack </> "source" </> name
      convDir = pack </> "converted" </> name
  assertDirExists srcDir
  assertNotExists convDir
  -- Find samples
  srcSamples <- matchSamples (T.pack name) "aif" srcDir
  when (Seq.null srcSamples) (fail "Found no samples")
  -- Convert them
  -- TODO
  -- Write them out
  createDirectoryIfMissing True convDir
  -- TODO
  -- Initialize instrument
  -- TODO

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists fp = do
  exists <- doesPathExist fp
  when exists (removeDirectoryRecursive fp)

runClean :: PackRef -> IO ()
runClean pr = do
  PackRef pack name <- canonPackRef pr
  -- Resolve dirs
  let convDir = pack </> "converted" </> name
  -- Make sure
  putStrLn ("Are you sure you want to clean pack " ++ convDir ++ " ? [no|yes] ")
  ans <- getLine
  case ans of
    "yes" -> do
      putStrLn "Cleaning..."
      removeDirectoryIfExists convDir
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
