{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scrapti.Patches.Sfz
  ( SfzVal (..)
  , sfzValText
  , sfzValInt
  , sfzValFloat
  , textSfzVal
  , sfzValSimilar
  , SfzAttrs
  , sfzAttrsSimilar
  , SfzSection (..)
  , sfzSectionSimilar
  , SfzFile (..)
  , sfzFileSimilar
  , parseSfz
  , renderSfz
  , findSfzSection
  , replaceSfzSection
  )
where

import Control.Monad (unless)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Data.Char (isAlpha, isDigit)
import Data.Foldable (for_, toList)
import Data.Functor.Classes (Eq1 (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Pretty (..))
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as PT
import Text.Read (readEither)

{-
SFZ params
https://sfzformat.com/opcodes/

<control> - first (optional) section, can have default_path for samples
‹global› - params for all regions
<group> - params for group of regions

in <region>
pan = -100 to 100 as float %

Filter:
fil_type = {lpf,hpf,bpf}_2p
cutoff = 0 to SampleRate / 2 as float Hz
resonance = 0 to 40 as float Db

What corresponds to
reverb send, delay send, overdrive, bit depth?
Don't bother

Example region:
<region>
lokey=0
hikey=127
pitch_keycenter=36
loop_mode=loop_continuous
loop_start=11181
loop_end=33096
sample=DX-EPiano1-C1.wav
-}

projectRational :: Rational -> Either Integer Double
projectRational r = if denominator r == 1 then Left (numerator r) else Right (fromRational r)

showRational :: Rational -> String
showRational = either show show . projectRational

prettyRational :: Rational -> P.Doc a
prettyRational = either pretty pretty . projectRational

data SfzVal
  = SfzValText !Text
  | SfzValInt !Integer
  | SfzValFloat !Rational
  deriving stock (Eq, Show)

sfzValText :: SfzVal -> Maybe Text
sfzValText =
  Just . \case
    SfzValText t -> t
    SfzValInt i -> T.pack (show i)
    SfzValFloat r -> T.pack (showRational r)

sfzValInt :: SfzVal -> Maybe Integer
sfzValInt = \case
  SfzValInt i -> Just i
  _ -> Nothing

sfzValFloat :: SfzVal -> Maybe Rational
sfzValFloat = \case
  SfzValInt i -> Just (fromInteger i)
  SfzValFloat r -> Just r
  _ -> Nothing

textSfzVal :: Pretty a => a -> SfzVal
textSfzVal = SfzValText . PT.renderStrict . P.layoutCompact . pretty

sfzValSimilar :: SfzVal -> SfzVal -> Bool
sfzValSimilar x y =
  case (x, y) of
    (SfzValFloat xf, _) -> Just xf == sfzValFloat y
    (_, SfzValFloat yf) -> sfzValFloat x == Just yf
    (SfzValInt xi, _) -> Just xi == sfzValInt y
    (_, SfzValInt yi) -> sfzValInt x == Just yi
    (SfzValText xt, _) -> Just xt == sfzValText y

instance Pretty SfzVal where
  pretty = \case
    SfzValText x -> pretty x
    SfzValInt x -> pretty x
    SfzValFloat x -> prettyRational x

type SfzAttrs = Map Text SfzVal

sfzAttrsSimilar :: SfzAttrs -> SfzAttrs -> Bool
sfzAttrsSimilar = liftEq sfzValSimilar

data SfzSection = SfzSection
  { ssName :: !Text
  , ssAttrs :: !SfzAttrs
  }
  deriving stock (Eq, Show)

sfzSectionSimilar :: SfzSection -> SfzSection -> Bool
sfzSectionSimilar (SfzSection n1 m1) (SfzSection n2 m2) = n1 == n2 && sfzAttrsSimilar m1 m2

instance Pretty SfzSection where
  pretty (SfzSection name attrs) = P.vsep (nameDoc : attrsDocs)
   where
    nameDoc = P.hcat ["<", pretty name, ">"]
    attrsDocs = fmap (\(n, v) -> P.hcat [pretty n, "=", pretty v]) (Map.toList attrs)

newtype SfzFile = SfzFile {unSfzFile :: Seq SfzSection}
  deriving stock (Eq, Show)

sfzFileSimilar :: SfzFile -> SfzFile -> Bool
sfzFileSimilar (SfzFile s1) (SfzFile s2) = liftEq sfzSectionSimilar s1 s2

instance Pretty SfzFile where
  pretty (SfzFile sections) = P.sep (fmap pretty (toList sections))

data SfzCtx = SfzCtx
  { scSections :: !(Seq SfzSection)
  , scCurrentSection :: !(Maybe SfzSection)
  }
  deriving stock (Eq, Show)

emptySfzCtx :: SfzCtx
emptySfzCtx = SfzCtx Seq.empty Nothing

newtype SfzParser a = SfzParser {unSfzParser :: StateT SfzCtx (Except String) a}
  deriving newtype (Functor, Applicative, Monad, MonadState SfzCtx)

runSfzParser :: SfzParser a -> SfzCtx -> Either String (a, SfzCtx)
runSfzParser m c = runExcept (runStateT (unSfzParser m) c)

instance MonadFail SfzParser where
  fail = SfzParser . throwError

parseSfzM :: Text -> SfzParser SfzFile
parseSfzM contents = for_ (T.lines contents) parseSfzLineM *> fmap SfzFile resolveSectionsM

parseSfzLineM :: Text -> SfzParser ()
parseSfzLineM line =
  unless (T.null line) $ do
    case (T.head line, T.last line) of
      ('<', '>') -> newSectionM (T.init (T.tail line))
      _ -> case T.splitOn "=" line of
        [key, valStr] -> do
          val <- parseValM valStr
          addAttrM key val
        _ -> fail ("invalid sfz line: " ++ T.unpack line)

readM :: Read a => (a -> b) -> Text -> SfzParser b
readM f t =
  let s = T.unpack t
  in  case readEither s of
        Left err -> fail ("Failed to read attr val " ++ s ++ ": " ++ err)
        Right val -> pure $! f val

parseValM :: Text -> SfzParser SfzVal
parseValM valStr
  | T.elem '.' valStr && not (T.any isAlpha valStr) = readM SfzValFloat valStr
  | not (T.null valStr) && isDigit (T.head valStr) = readM SfzValInt valStr
  | otherwise = pure $! SfzValText valStr

addAttrM :: Text -> SfzVal -> SfzParser ()
addAttrM key val = do
  SfzCtx {..} <- get
  case scCurrentSection of
    Nothing -> fail ("attribute with no section: " ++ T.unpack key)
    Just (SfzSection name attrs) ->
      put $! SfzCtx scSections (Just (SfzSection name (Map.insert key val attrs)))

newSectionM :: Text -> SfzParser ()
newSectionM name = do
  sections <- resolveSectionsM
  put $! SfzCtx sections (Just (SfzSection name Map.empty))

resolveSectionsM :: SfzParser (Seq SfzSection)
resolveSectionsM = do
  SfzCtx {..} <- get
  pure $! maybe scSections (scSections :|>) scCurrentSection

parseSfz :: Text -> Either String SfzFile
parseSfz contents = fmap fst (runSfzParser (parseSfzM contents) emptySfzCtx)

renderSfz :: SfzFile -> Text
renderSfz sf = PT.renderStrict (P.layoutPretty P.defaultLayoutOptions (pretty sf))

findSfzSection :: Text -> SfzFile -> Maybe SfzSection
findSfzSection name (SfzFile sections) = fmap (Seq.index sections) (Seq.findIndexL (\x -> ssName x == name) sections)

replaceSfzSection :: Text -> SfzFile -> SfzSection -> Maybe SfzFile
replaceSfzSection name (SfzFile sections) val =
  fmap (\ix -> SfzFile (Seq.update ix val sections)) (Seq.findIndexL (\x -> ssName x == name) sections)
