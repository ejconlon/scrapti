module Main (main) where

import Scrapti.Sfont qualified as S
import Scrapti.Riff qualified as R
import System.Environment (getArgs)
import System.Directory (getHomeDirectory, doesDirectoryExist)
import System.FilePath ((</>), isExtensionOf)
import Control.Monad (unless, foldM)
import System.Directory.PathWalk (pathWalkAccumulate)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Dahdit qualified as D
import Data.String (fromString)
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State.Strict (StateT (..), execStateT)

type FoldM a b = ReaderT a (StateT b IO)

runFoldM :: FoldM a b () -> b -> Seq a -> IO b
runFoldM m = foldM (\b a -> execStateT (runReaderT m a) b)

mkTB16 :: String -> D.TermBytes16
mkTB16 = D.TermBytes16 . fromString

mkInfoChunk :: String -> S.InfoChunk
mkInfoChunk name = S.InfoChunk $ R.KnownListChunk $ Seq.fromList
  [ S.InfoVersion 2 1
  , S.InfoBankName (mkTB16 name)
  ]

gatherFiles :: FilePath -> IO (Seq FilePath)
gatherFiles root = pathWalkAccumulate root $ \dir _ files -> do
  pure (Seq.fromList (fmap (dir </>) (filter (isExtensionOf "sf2") files)))

sampleOffsets :: Seq S.Sfont -> Seq D.ElemCount
sampleOffsets = flip Seq.scanl 0 $ \i sf ->
  case R.kocItem (S.unSdtaChunk (S.sfontSdta sf)) of
    Nothing -> i
    Just (S.Sdta hi mlo) -> case mlo of
      Just _ -> error "won't handle 24-bit samples"
      Nothing -> i + D.lengthLiftedPrimArray hi

split :: S.Sfont -> IO (Seq S.Sfont)
split = undefined

merge :: Seq S.Sfont -> IO S.Sfont
merge = undefined

readSfont :: FilePath -> IO S.Sfont
readSfont file = do
  (esf, _) <- D.decodeFileEnd @S.Sfont file
  case esf of
    Left e -> fail ("Failed to parse SF @ " ++ file ++ " : " ++ show e)
    Right sf -> pure sf

gatherSfonts :: FilePath -> IO (Seq (FilePath, S.Sfont))
gatherSfonts path = do
  files <- gatherFiles path
  for files (\file -> fmap (file,) (readSfont file))

normPath :: FilePath -> IO FilePath
normPath = \case
  '~':rest -> fmap (</> rest) getHomeDirectory
  path -> pure path

assertDirExists :: FilePath -> IO ()
assertDirExists path = do
  pathExists <- doesDirectoryExist path
  unless pathExists (fail ("Path does not exist: " ++ path))

data Info = Info
  deriving stock (Eq, Ord, Show)

emptyInfo :: Info
emptyInfo = Info

addInfo :: FoldM S.Sfont Info ()
addInfo = do
  pure ()

mkInfo :: Seq S.Sfont -> IO Info
mkInfo = runFoldM addInfo emptyInfo

debugSfont :: FilePath -> S.Sfont -> IO ()
debugSfont file sf =  do
  putStrLn "+ file"
  putStrLn file
  putStrLn "+ sizes"
  putStr "Info size: "
  print (Seq.length (R.klcItems (S.unInfoChunk (S.sfontInfo sf))))
  putStr "Pdta size: "
  print (Seq.length (R.klcItems (S.unPdtaChunk (S.sfontPdta sf))))
  -- putStrLn "+ info"
  -- print (S.sfontInfo sf)
  putStrLn "+ pdta list"
  for_ (R.klcItems (S.unPdtaChunk (S.sfontPdta sf))) $ \pb -> do
    case pb of
      S.PdtaBlockPhdr _ -> do
        putStrLn "++ phdr"
      S.PdtaBlockBag _ _ -> do
        putStrLn "++ bag"
      S.PdtaBlockMod _ _ -> do
        putStrLn "++ mod"
      S.PdtaBlockGen _ _ -> do
        putStrLn "++ gen"
      S.PdtaBlockInst _ -> do
        putStrLn "++ inst"
      S.PdtaBlockShdr _ -> do
        putStrLn "++ shdr"

debugSfonts :: Seq (FilePath, S.Sfont) -> IO ()
debugSfonts sfs =  do
  for_ sfs $ \(file, sf) -> do
    putStrLn (replicate 40 '*')
    debugSfont file sf

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      newPath <- normPath path
      assertDirExists newPath
      sfs <- gatherSfonts newPath
      debugSfonts sfs
    _ -> fail "Use: sf2merge [directory]"

{-
 - Notes:
 - Need to merge samples in sample chunk
 - Can just enforce all 16bit samples for now
 - PdtaChunk is a KnownListChunk of PdtaBlock
 -
data PdtaBlock
  = PdtaBlockPhdr !(Seq Phdr)
  | PdtaBlockBag !PdtaCat !(Seq Bag)
  | PdtaBlockMod !PdtaCat !(Seq Mod)
  | PdtaBlockGen !PdtaCat !(Seq Gen)
  | PdtaBlockInst !(Seq Inst)
  | PdtaBlockShdr !(Seq Shdr)
 -}
