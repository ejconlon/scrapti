{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Patches.Common where

import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit)
import Text.Read (readMaybe)

data NoteName =
    NoteNameC
  | NoteNameCS
  | NoteNameD
  | NoteNameDS
  | NoteNameE
  | NoteNameF
  | NoteNameFS
  | NoteNameG
  | NoteNameGS
  | NoteNameA
  | NoteNameAS
  | NoteNameB
  deriving stock (Eq, Ord, Enum, Bounded, Show)

newtype Octave = Octave { unOctave :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

-- | A note split into octave and name
data OctNote = OctNote
  { onOctave :: !Octave
  , onName :: !NoteName
  } deriving stock (Eq, Ord, Show)

octNoteIsMidi :: OctNote -> Bool
octNoteIsMidi (OctNote (Octave oct) name) =
  if
    | oct == -1 -> name == NoteNameC
    | oct == 9 -> name <= NoteNameGS
    | otherwise -> oct >= 0 && oct <= 8

-- | An integral note type that can represent notes outside the MIDI scale.
newtype LinNote = LinNote { unLinNote :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

linIsMidiNote :: LinNote -> Bool
linIsMidiNote (LinNote n) = n >= 0 && n < 127

newtype Interval = Interval { unInterval :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

data NoteAssoc = NoteAssoc
  { naParsed :: !OctNote
  , naRaw :: !LinNote
  } deriving stock (Eq, Show)

linAddInterval :: Interval -> LinNote -> LinNote
linAddInterval (Interval i) (LinNote n) = LinNote (i + n)

linToOct :: LinNote -> OctNote
linToOct (LinNote n) = OctNote (Octave (div n 12 - 1)) (toEnum (mod n 12))

octToLin :: OctNote -> LinNote
octToLin (OctNote (Octave oct) nn) = LinNote (((oct + 1) * 12) + fromEnum nn)

octAddInterval :: Interval -> OctNote -> OctNote
octAddInterval i = linToOct . linAddInterval i . octToLin

noteAssoc :: OctNote -> NoteAssoc
noteAssoc oct = NoteAssoc oct (octToLin oct)

newtype Scale = Scale
  { scaleIntervals :: Seq Interval
  } deriving stock (Show)
    deriving newtype (Eq)

data ScaleClassifier = ScaleClassifier
  { scRoot :: !NoteName
  , scMembers :: !(Set NoteName)
  } deriving stock (Eq, Show)

parseNoteName :: Text -> Maybe (NoteName, Int)
parseNoteName = \case
  "Cb" -> Just (NoteNameB, -1)
  "C" -> Just (NoteNameC, 0)
  "C#" -> Just (NoteNameCS, 0)
  "Db" -> Just (NoteNameCS, 0)
  "D" -> Just (NoteNameD, 0)
  "D#" -> Just (NoteNameDS, 0)
  "Eb" -> Just (NoteNameDS, 0)
  "E" -> Just (NoteNameE, 0)
  "E#" -> Just (NoteNameF, 0)
  "Fb" -> Just (NoteNameE, 0)
  "F" -> Just (NoteNameF, 0)
  "F#" -> Just (NoteNameFS, 0)
  "Gb" -> Just (NoteNameFS, 0)
  "G" -> Just (NoteNameG, 0)
  "G#" -> Just (NoteNameGS, 0)
  "Ab" -> Just (NoteNameGS, 0)
  "A" -> Just (NoteNameA, 0)
  "A#" -> Just (NoteNameAS, 0)
  "Bb" -> Just (NoteNameAS, 0)
  "B" -> Just (NoteNameB, 0)
  "B#" -> Just (NoteNameC, 1)
  _ -> Nothing

parseNote :: Text -> Maybe OctNote
parseNote ns = do
  let (noteRaw, octRaw) = T.break isDigit ns
  (nn, off) <- parseNoteName noteRaw
  octStart <- readMaybe @Int (T.unpack octRaw)
  let !oct = Octave (octStart + off)
  pure $! OctNote oct nn
