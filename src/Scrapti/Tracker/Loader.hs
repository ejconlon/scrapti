module Scrapti.Tracker.Loader where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, (>=>))
import Dahdit (Binary (..), GetError, decode, encodeFile)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Scrapti.Tracker.Checked (failCheckedCode)
import Scrapti.Tracker.Mt (Mt (..))
import Scrapti.Tracker.Mtp (Mtp (..))
import Scrapti.Tracker.Pti (Pti (ptiHeader))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory)
import System.FilePath ((</>))
import Text.Read (readEither)
import Text.Regex.TDFA ((=~~))

data Project s i p = Project
  { projectSong :: !s
  , projectInsts :: !(Map Int i)
  , projectPats :: !(Map Int p)
  }
  deriving stock (Eq, Show)

data Res
  = ResSong
  | ResInst !Int
  | ResPat !Int
  deriving stock (Eq, Show)

data LoadError
  = LoadErrorMissingProject !FilePath
  | LoadErrorMissingRes !FilePath !Res !FilePath
  | LoadErrorMalformedResPath !FilePath !FilePath !String
  | LoadErrorBadResGet !FilePath !Res !FilePath !GetError
  deriving stock (Eq, Show)

instance Exception LoadError

type BareProject = Project FilePath (String, FilePath) FilePath

type RichProject = Project Mt (String, Pti) Mtp

newtype M a = M {unM :: Either String a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail M where
  fail = M . Left

parseInstPart :: FilePath -> Either String (Int, String)
parseInstPart part = unM $ do
  (_, _, _, [numStr, name]) <- part =~~ "^([[:digit:]]+) ([^\\.]+)\\.pti" :: M (String, String, String, [String])
  num <- M $ readEither @Int numStr
  pure (num, name)

renderInstPart :: Int -> String -> FilePath
renderInstPart num name = "instruments" </> show num ++ " " ++ name ++ ".pti"

parsePatPart :: FilePath -> Either String (Int, ())
parsePatPart part = unM $ do
  (_, _, _, [numStr]) <- part =~~ "^pattern_0?([[:digit:]]+)\\.mtp" :: M (String, String, String, [String])
  num <- M $ readEither @Int numStr
  pure (num, ())

renderPatPart :: Int -> FilePath
renderPatPart num = "patterns" </> "pattern_" ++ (if num < 9 then "0" else "") ++ show num ++ ".mtp"

crawlThingDir :: (FilePath -> Either String (Int, n)) -> FilePath -> FilePath -> IO (Map Int (n, FilePath))
crawlThingDir extract projDir thingsPart = do
  let thingsDir = projDir </> thingsPart
  thingsExists <- doesDirectoryExist thingsDir
  if thingsExists
    then do
      instParts <- listDirectory thingsDir
      fmap Map.fromList $ for instParts $ \part -> do
        let thingPart = thingsPart </> part
        case extract part of
          Left e -> throwIO (LoadErrorMalformedResPath projDir thingPart e)
          Right (i, n) -> pure (i, (n, thingPart))
    else pure Map.empty

loadBareProject :: FilePath -> IO BareProject
loadBareProject projDir = do
  projExists <- doesDirectoryExist projDir
  unless projExists (throwIO (LoadErrorMissingProject projDir))
  let songPart = "project.mt"
      songFile = projDir </> songPart
  songExists <- doesFileExist songFile
  unless songExists (throwIO (LoadErrorMissingRes projDir ResSong songPart))
  insts <- crawlThingDir parseInstPart projDir "instruments"
  pats <- fmap (fmap snd) (crawlThingDir parsePatPart projDir "patterns")
  pure (Project songPart insts pats)

runGetRes :: (Binary a) => FilePath -> Res -> FilePath -> IO a
runGetRes projDir res resPart = do
  let resFile = projDir </> resPart
  bs <- fmap BSS.toShort (BS.readFile resFile)
  (ea, _) <- decode bs
  case ea of
    Left e -> throwIO (LoadErrorBadResGet projDir res resPart e)
    Right a -> pure a

loadSong :: FilePath -> Project FilePath i p -> IO (Project Mt i p)
loadSong projDir proj = do
  s <- runGetRes projDir ResSong (projectSong proj)
  failCheckedCode "loaded song" (unMt s)
  pure $! proj {projectSong = s}

loadInstruments :: FilePath -> Project s (String, FilePath) p -> IO (Project s (String, Pti) p)
loadInstruments projDir proj = do
  m <- flip Map.traverseWithKey (projectInsts proj) $ \i (n, x) -> do
    y <- runGetRes projDir (ResInst i) x
    failCheckedCode "loaded instrument" (ptiHeader y)
    pure (n, y)
  pure $! proj {projectInsts = m}

loadPatterns :: FilePath -> Project s i FilePath -> IO (Project s i Mtp)
loadPatterns projDir proj = do
  m <- flip Map.traverseWithKey (projectPats proj) $ \i x -> do
    y <- runGetRes projDir (ResPat i) x
    failCheckedCode "loaded pattern" (unMtp y)
    pure y
  pure $! proj {projectPats = m}

enrichProject :: FilePath -> BareProject -> IO RichProject
enrichProject projPath =
  loadSong projPath >=> loadInstruments projPath >=> loadPatterns projPath

loadRichProject :: FilePath -> IO RichProject
loadRichProject projPath = loadBareProject projPath >>= enrichProject projPath

encodeRes :: (Binary a) => FilePath -> a -> FilePath -> IO ()
encodeRes projDir val resPath = encodeFile val (projDir </> resPath)

saveSong :: FilePath -> Project Mt i p -> IO ()
saveSong projDir proj = do
  let s = projectSong proj
  failCheckedCode "saved song" (unMt s)
  encodeRes projDir s "project.mt"

saveInstruments :: FilePath -> Project s (String, Pti) p -> IO ()
saveInstruments projDir proj = for_ (Map.toList (projectInsts proj)) $ \(i, (n, x)) -> do
  failCheckedCode "saved instrument" (ptiHeader x)
  encodeRes projDir x (renderInstPart i n)

savePatterns :: FilePath -> Project s i Mtp -> IO ()
savePatterns projDir proj = for_ (Map.toList (projectPats proj)) $ \(i, x) -> do
  failCheckedCode "saved pattern" (unMtp x)
  encodeRes projDir x (renderPatPart i)

data Overwrite
  = OverwriteNo
  | OverwriteYesReally
  deriving stock (Eq, Show)

saveRichProject :: Overwrite -> FilePath -> RichProject -> IO ()
saveRichProject ow projDir proj = do
  guardOverwrite ow projDir
  createDirectoryIfMissing True projDir
  createDirectoryIfMissing False (projDir </> "instruments")
  createDirectoryIfMissing False (projDir </> "patterns")
  saveSong projDir proj
  saveInstruments projDir proj
  savePatterns projDir proj

guardOverwrite :: Overwrite -> FilePath -> IO ()
guardOverwrite o fp = do
  exist <- doesPathExist fp
  case (o, exist) of
    (OverwriteNo, True) -> fail ("Aborting overwrite of " ++ fp)
    _ -> pure ()
