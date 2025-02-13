{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Midi.Notes where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data NoteName
  = NoteNameC
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

data NotePref = NotePrefSharp | NotePrefFlat
  deriving stock (Eq, Enum, Bounded, Show)

renderNoteName :: NotePref -> NoteName -> Text
renderNoteName pref nn =
  let sharp = case pref of NotePrefSharp -> True; _ -> False
  in  case nn of
        NoteNameC -> "C"
        NoteNameCS -> if sharp then "C#" else "Db"
        NoteNameD -> "D"
        NoteNameDS -> if sharp then "D#" else "Eb"
        NoteNameE -> "E"
        NoteNameF -> "F"
        NoteNameFS -> if sharp then "F#" else "Gb"
        NoteNameG -> "G"
        NoteNameGS -> if sharp then "G#" else "Ab"
        NoteNameA -> "A"
        NoteNameAS -> if sharp then "A#" else "Bb"
        NoteNameB -> "B"

newtype Octave = Octave {unOctave :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

-- | A note split into octave and name
data OctNote = OctNote
  { onOctave :: !Octave
  , onName :: !NoteName
  }
  deriving stock (Eq, Ord, Show)

octNoteIsMidi :: OctNote -> Bool
octNoteIsMidi (OctNote (Octave oct) name) =
  if
    | oct == -1 -> name == NoteNameC
    | oct == 9 -> name <= NoteNameG
    | otherwise -> oct >= 0 && oct <= 8

-- | An integral note type that can represent notes outside the MIDI scale.
newtype LinNote = LinNote {unLinNote :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- Midi notes are between 0 (C-1) and 127 (G9)
-- Piano notes are between 21 (A0) and 108 (C8)
linIsMidiNote :: LinNote -> Bool
linIsMidiNote (LinNote n) = n >= 0 && n < 127

linFreq :: LinNote -> Double
linFreq (LinNote n) = 440 * (2 ** ((fromIntegral n - 69) / 12))

newtype Interval = Interval {unInterval :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

data NoteAssoc = NoteAssoc
  { naParsed :: !OctNote
  , naRaw :: !LinNote
  }
  deriving stock (Eq, Show)

linAddInterval :: Interval -> LinNote -> LinNote
linAddInterval (Interval i) (LinNote n) = LinNote (i + n)

linToOct :: LinNote -> OctNote
linToOct (LinNote n) = OctNote (Octave (div n 12 - 1)) (toEnum (mod n 12))

linSubInterval :: LinNote -> LinNote -> Interval
linSubInterval (LinNote a) (LinNote b) = Interval (a - b)

octToLin :: OctNote -> LinNote
octToLin (OctNote (Octave oct) nn) = LinNote (((oct + 1) * 12) + fromEnum nn)

octAddInterval :: Interval -> OctNote -> OctNote
octAddInterval i = linToOct . linAddInterval i . octToLin

noteAssoc :: OctNote -> NoteAssoc
noteAssoc oct = NoteAssoc oct (octToLin oct)

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
  let (noteRaw, octRaw) = T.break (\c -> isDigit c || c == '-') ns
  (nn, off) <- parseNoteName noteRaw
  octStart <- readMaybe @Int (T.unpack octRaw)
  let !oct = Octave (octStart + off)
  pure $! OctNote oct nn

renderNote :: NotePref -> OctNote -> Text
renderNote pref (OctNote o nn) =
  renderNoteName pref nn <> T.pack (show (unOctave o))
