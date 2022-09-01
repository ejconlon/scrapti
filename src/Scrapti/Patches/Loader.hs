{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Patches.Loader
  ( Sample (..)
  , matchSamples
  , defaultInst
  ) where

import Scrapti.Midi.Notes (OctNote, parseNote)
import Scrapti.Midi.Msg (Velocity (..))
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (listDirectory)
import Text.Regex.TDFA ((=~~))
import Text.Read (readMaybe)
import Data.Maybe (maybeToList)
import Scrapti.Patches.Inst (InstSpec)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Sample = Sample
  { samplePath :: !FilePath
  , sampleNote :: !OctNote
  , sampleVel :: !(Maybe Velocity)
  , sampleUniq :: !(Maybe Text)
  } deriving stock (Eq, Show)

newtype F a = F { unF :: Either String a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail F where
  fail = F . Left

matchFiles :: Text -> Text -> FilePath -> IO [(FilePath, [Text])]
matchFiles prefix fileExt dir = do
  let pat = "^" ++ T.unpack prefix ++ "([^\\\\\\.]+)\\." ++ T.unpack fileExt ++ "$"
  files <- listDirectory dir
  pure $! do
    fp <- files
    let ex = fp =~~ pat :: F (String, String, String, [String])
    case unF ex of
      Right (_, _, _, [dashed]) ->
        let parts = T.splitOn "-" (T.pack dashed)
        in [(dir </> fp, parts)]
      _ -> []

parseVel :: Text -> Maybe Velocity
parseVel v
  | not (T.null v) && T.head v == 'V' = fmap Velocity (readMaybe (T.unpack v))
  | otherwise = Nothing

parseSample :: FilePath -> [Text] -> Maybe Sample
parseSample fp parts = do
  (noteStr, mayVelStr, mayUniqStr) <- case parts of
    [noteStr] -> Just (noteStr, Nothing, Nothing)
    [noteStr, velStr] -> Just (noteStr, Just velStr, Nothing)
    [noteStr, velStr, uniqStr] -> Just (noteStr, Just velStr, Just uniqStr)
    _ -> Nothing
  octNote <- parseNote noteStr
  let mayVel = mayVelStr >>= parseVel
  pure $! Sample fp octNote mayVel mayUniqStr

matchSamples :: Text -> Text -> FilePath -> IO (Seq Sample)
matchSamples prefix fileExt dir = do
  pairs <- matchFiles prefix fileExt dir
  pure $! Seq.fromList $ pairs >>= maybeToList . uncurry parseSample

defaultInst :: Seq Sample -> InstSpec FilePath
defaultInst = error "TODO"
