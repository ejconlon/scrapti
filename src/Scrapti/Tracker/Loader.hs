module Scrapti.Tracker.Loader where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, (>=>))
import Dahdit (Binary (..), GetError, runGet, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Scrapti.Tracker.Mt (Mt)
import Scrapti.Tracker.Mtp (Mtp)
import Scrapti.Tracker.Pti (Pti)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory)
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

-- data WithPath a = WithPath !FilePath !a
--   deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable)

-- data WithName a = WithName !String !a
--   deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable)

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

renderInstPart :: Int -> String -> FilePath
renderInstPart num name = show num ++ " " ++ name ++ ".pti"

parsePatPart :: FilePath -> Either String (Int, ())
parsePatPart part = unM $ do
  (_, _, _, [numStr]) <- part =~~ "^pattern_0?([[:digit:]]+)\\.mtp" :: M (String, String, String, [String])
  num <- M $ readEither @Int numStr
  pure (num, ())

renderPatPart :: Int -> FilePath
renderPatPart num = "pattern_" ++ if num < 9 then "0" else "" ++ show num ++ ".mtp"

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

runGetRes :: Binary a => FilePath -> Res -> FilePath -> IO a
runGetRes pp rs rp = do
  let rf = pp </> rp
  bs <- fmap BSS.toShort (BS.readFile rf)
  let (ea, _) = runGet get bs
  case ea of
    Left e -> throwIO (LoadErrorBadResGet pp rs rp e)
    Right a -> pure a

loadSong :: Project FilePath i p -> IO (Project Mt i p)
loadSong p = fmap (\s -> p { projectSong = s }) (runGetRes (projectPath p) ResSong (projectSong p))

loadInstruments :: Project s (String, FilePath) p -> IO (Project s (String, Pti) p)
loadInstruments p =
  fmap (\m -> p { projectInsts = m }) (Map.traverseWithKey (\i (n, x) -> fmap (n,) (runGetRes (projectPath p) (ResInst i) x)) (projectInsts p))

loadPatterns :: Project s i FilePath -> IO (Project s i Mtp)
loadPatterns p =
  fmap (\m -> p { projectPats = m }) (Map.traverseWithKey (runGetRes (projectPath p) . ResPat) (projectPats p))

enrichProject :: BareProject -> IO RichProject
enrichProject = loadSong >=> loadInstruments >=> loadPatterns

loadRichProject :: FilePath -> IO RichProject
loadRichProject = loadBareProject >=> enrichProject

runPutRes :: Binary a => FilePath -> a -> FilePath -> IO ()
runPutRes pp a rp =
  let rf = pp </> rp
      bs = BSS.fromShort (runPut (put a))
  in BS.writeFile rf bs

saveSong :: Project Mt i p -> IO ()
saveSong p = runPutRes (projectPath p) (projectSong p) "project.mt"

saveInstruments :: Project s (String, Pti) p -> IO ()
saveInstruments p = for_ (Map.toList (projectInsts p)) (\(i, (n, x)) -> runPutRes (projectPath p) x (renderInstPart i n))

savePatterns :: Project s i Mtp -> IO ()
savePatterns p = for_ (Map.toList (projectPats p)) (\(i, x) -> runPutRes (projectPath p) x (renderPatPart i))

data Overwrite =
    OverwriteNo
  | OverwriteYesReally
  deriving stock (Eq, Show)

saveRichProject :: Overwrite -> RichProject -> IO ()
saveRichProject o p = do
  let root = projectPath p
  guardOverwrite o root
  createDirectoryIfMissing True root
  createDirectoryIfMissing True (root </> "instruments")
  createDirectoryIfMissing True (root </> "patterns")
  saveSong p
  saveInstruments p
  savePatterns p

guardOverwrite :: Overwrite -> FilePath -> IO ()
guardOverwrite o fp = do
  exist <- doesPathExist fp
  case (o, exist) of
    (OverwriteNo, True) -> fail ("Aborting overwrite of " ++ fp)
    _ -> pure ()
