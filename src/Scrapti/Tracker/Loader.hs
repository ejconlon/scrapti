module Scrapti.Tracker.Loader where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, (>=>))
import Dahdit (Binary (..), GetError, runGet)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Scrapti.Tracker.Mt (Mt)
import Scrapti.Tracker.Mtp (Mtp)
import Scrapti.Tracker.Pti (Pti)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Text.Read (readEither)
import Text.Regex.TDFA ((=~~))

data Project s i p = Project
  { projectPath :: !FilePath
  , projectSong :: !s
  , projectInsts :: !(Map Int i)
  , projectPats :: !(Map Int p)
  } deriving stock (Eq, Show)

data Res =
    ResSong
  | ResInst !Int
  | ResPat !Int
  deriving stock (Eq, Show)


data LoadError =
    LoadErrorMissingProject !FilePath
  | LoadErrorMissingRes !FilePath !Res !FilePath
  | LoadErrorMalformedResPath !FilePath !FilePath !String
  | LoadErrorBadResGet !FilePath !Res !FilePath !GetError
  deriving stock (Eq, Show)

instance Exception LoadError

type BareProject = Project FilePath (String, FilePath) FilePath
type RichProject = Project Mt (String, Pti) Mtp

newtype M a = M { unM :: Either String a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail M where
  fail = M . Left

parseInstPart :: FilePath -> Either String (Int, String)
parseInstPart part = unM $ do
  (_, _, _, [numStr, name]) <- part =~~ "^([[:digit:]]+) ([^\\.]+)\\.pti" :: M (String, String, String, [String])
  num <- M $ readEither @Int numStr
  pure (num, name)

parsePatPart :: FilePath -> Either String (Int, ())
parsePatPart part = unM $ do
  (_, _, _, [numStr]) <- part =~~ "^pattern_0?([[:digit:]]+)\\.mtp" :: M (String, String, String, [String])
  num <- M $ readEither @Int numStr
  pure (num, ())

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
  pure (Project projDir songPart insts pats)

parseBinary :: Binary a => FilePath -> Res -> FilePath -> IO a
parseBinary pp rs rp = do
  let rf = pp </> rp
  bs <- fmap BSS.toShort (BS.readFile rf)
  let (ea, _) = runGet get bs
  case ea of
    Left e -> throwIO (LoadErrorBadResGet pp rs rp e)
    Right a -> pure a

loadSong :: Project FilePath i p -> IO (Project Mt i p)
loadSong p = fmap (\s -> p { projectSong = s }) (parseBinary (projectPath p) ResSong (projectSong p))

loadInstruments :: Project s (String, FilePath) p -> IO (Project s (String, Pti) p)
loadInstruments p =
  fmap (\m -> p { projectInsts = m }) (Map.traverseWithKey (\i (n, x) -> fmap (n,) (parseBinary (projectPath p) (ResInst i) x)) (projectInsts p))

loadPatterns :: Project s i FilePath -> IO (Project s i Mtp)
loadPatterns p =
  fmap (\m -> p { projectPats = m }) (Map.traverseWithKey (parseBinary (projectPath p) . ResPat) (projectPats p))

enrichProject :: BareProject -> IO RichProject
enrichProject = loadSong >=> loadInstruments >=> loadPatterns

loadRichProject :: FilePath -> IO RichProject
loadRichProject = loadBareProject >=> enrichProject
