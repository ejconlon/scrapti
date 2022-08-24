{-# LANGUAGE OverloadedStrings #-}

module Scrapti.Midi.Msg
  ( Channel (..)
  , Note (..)
  , Velocity (..)
  , ControlNum (..)
  , ControlVal (..)
  , Pressure (..)
  , ProgramNum (..)
  , PitchBend (..)
  , noteOn
  , noteOff
  , ChanVoiceData (..)
  , MidiMsg (..)
  , MidiEvent (..)
  , MidiTrack (..)
  , MidiFile (..)
  ) where

import Control.Monad (unless)
import Dahdit (Binary (..), BinaryRep (..), ByteCount, ByteSized (..), ExactBytes (..), Get, StaticByteSized (..),
               ViaBinaryRep (..), ViaGeneric (..), ViaStaticByteSized (..), Word16BE (..), byteSizeFoldable,
               getByteString, getLookAhead, getSeq, getWord8, putByteString, putSeq, putWord8)
import Data.Bits (Bits (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Int (Int16)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Word (Word16, Word32, Word8)
import GHC.Generics (Generic)

newtype Channel = Channel { unChannel :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype Note = Note { unNote :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype Velocity = Velocity { unVelocity :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype ControlNum = ControlNum { unControlNum :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype ControlVal = ControlVal { unControlVal :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype Pressure = Pressure { unPressure :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype ProgramNum = ProgramNum { unProgramNum :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype PitchBend = PitchBend { unPitchBend :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype Song = Song { unSong :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype Position = Position { unPosition :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype Manf = Manf { unManf :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

eduManf :: Manf
eduManf = Manf 0x7D

data QuarterTime =
    QTFramesLow !Word8
  | QTFramesHigh !Word8
  | QTSecondsLow !Word8
  | QTSecondsHigh !Word8
  | QTMinutesLow !Word8
  | QTMinutesHigh !Word8
  | QTHoursLow !Word8
  | QTHoursHigh !Word8
  deriving stock (Eq, Show, Generic)

quarterTimeKey :: QuarterTime -> Word8
quarterTimeKey = \case
  QTFramesLow _ -> 0x0
  QTFramesHigh _ -> 0x1
  QTSecondsLow _ -> 0x2
  QTSecondsHigh _ -> 0x3
  QTMinutesLow _ -> 0x4
  QTMinutesHigh _ -> 0x5
  QTHoursLow _ -> 0x6
  QTHoursHigh _ -> 0x7

quarterTimeValue :: QuarterTime -> Word8
quarterTimeValue = \case
  QTFramesLow w -> w
  QTFramesHigh w -> w
  QTSecondsLow w -> w
  QTSecondsHigh w -> w
  QTMinutesLow w -> w
  QTMinutesHigh w -> w
  QTHoursLow w -> w
  QTHoursHigh w -> w

noteOn :: Channel -> Note -> Velocity -> MidiMsg
noteOn c k v = MidiMsgChanVoice (ChanVoiceMsg c (ChanVoiceNoteOnOff k v))

noteOff :: Channel -> Note -> MidiMsg
noteOff c k = noteOn c k 0

data ChanVoiceData =
    ChanVoiceNoteOnOff !Note !Velocity
  | ChanVoicePolyAftertouch !Note !Pressure
  | ChanVoiceCC !ControlNum !ControlVal
  | ChanVoiceProgramChange !ProgramNum
  | ChanVoiceAftertouch !Pressure
  | ChanVoicePitchWheel !PitchBend
  deriving stock (Eq, Show, Generic)

data ChanVoiceMsg =
  ChanVoiceMsg !Channel !ChanVoiceData
  deriving stock (Eq, Show, Generic)

newtype SysExString = SysExString { unSysExString :: ShortByteString }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance ByteSized SysExString where
  byteSize = fromIntegral . BSS.length . unSysExString

findDataLength :: Get ByteCount
findDataLength = getLookAhead (go 0) where
  go !i = do
    w <- getWord8
    if w .&. 0x70 == 0
      then go (i + 1)
      else pure i

instance Binary SysExString where
  get = fmap SysExString (findDataLength >>= getByteString)
  put (SysExString ss) = putByteString ss

data MidiStatus =
    MidiStatusChanVoice !Channel
  | MidiStatusChanMode !Channel
  | MidiStatusSysEx
  | MidiStatusSysCommon !Word8
  | MidiStatusSysRt !Word8
  deriving stock (Eq, Show)
  deriving (ByteSized) via (ViaStaticByteSized MidiStatus)

instance StaticByteSized MidiStatus where
  staticByteSize _ = 1

instance Binary MidiStatus where
  get = error "TODO"
  put = error "TODO"

-- TODO(ejconlon) Implement ChannelMode message
-- https://www.midi.org/specifications/item/table-1-summary-of-midi-message
-- Also break this into status byte + data bytes by category
data MidiMsg =
    MidiMsgChanVoice !ChanVoiceMsg
  | MidiMsgSysex !Manf !SysExString
  | MidiMsgQuarterFrame !QuarterTime
  | MidiMsgSongPosition !Position
  | MidiMsgSongSelect !Song
  | MidiMsgTuneRequest
  | MidiMsgSrtClock
  | MidiMsgSrtStart
  | MidiMsgSrtContinue
  | MidiMsgSrtStop
  | MidiMsgActiveSensing
  | MidiMsgReset
  deriving stock (Eq, Show, Generic)

instance ByteSized MidiMsg where
  byteSize mp =
    1 + case mp of
      MidiMsgChanVoice (ChanVoiceMsg _ dat) ->
        case dat of
          ChanVoiceNoteOnOff _ _ -> 2
          ChanVoicePolyAftertouch _ _ -> 2
          ChanVoiceCC _ _ -> 2
          ChanVoiceProgramChange _ -> 1
          ChanVoiceAftertouch _ -> 1
          ChanVoicePitchWheel _ -> 2
      MidiMsgSysex _ sbs -> 1 + byteSize sbs
      MidiMsgQuarterFrame _ -> 1
      MidiMsgSongPosition _ -> 2
      MidiMsgSongSelect _ -> 1
      MidiMsgTuneRequest -> 0
      MidiMsgSrtClock -> 0
      MidiMsgSrtStart -> 0
      MidiMsgSrtContinue -> 0
      MidiMsgSrtStop -> 0
      MidiMsgActiveSensing -> 0
      MidiMsgReset -> 0

instance Binary MidiMsg where
  get = do
    x <- get @Word8
    error "TODO"

  put = \case
    MidiMsgChanVoice (ChanVoiceMsg (Channel c) dat) ->
      let !d = min 15 c
      in case dat of
        ChanVoiceNoteOnOff (Note n) (Velocity v) -> do
          putWord8 (d .|. 0x90)
          putWord8 n
          putWord8 v
        ChanVoicePolyAftertouch (Note n) (Pressure p) -> do
          putWord8 (d .|. 0xA0)
          putWord8 n
          putWord8 p
        ChanVoiceCC (ControlNum cn) (ControlVal cv) -> do
          putWord8 (d .|. 0xB0)
          putWord8 cn
          putWord8 cv
        ChanVoiceProgramChange (ProgramNum pn) -> do
          putWord8 (d .|. 0xC0)
          putWord8 pn
        ChanVoiceAftertouch (Pressure p) -> do
          putWord8 (d .|. 0xD0)
          putWord8 p
        ChanVoicePitchWheel (PitchBend pb) -> do
          putWord8 (d .|. 0xE0)
          let !w = min 16383 (max 0 (pb + 8192))
          putWord8 (fromIntegral w .|. 0x7)
          putWord8 (fromIntegral (shiftR w 7))
    MidiMsgSysex (Manf m) ss -> do
      putWord8 0xF0
      putWord8 m
      put ss
    MidiMsgQuarterFrame qt -> do
      putWord8 0xF1
      putWord8 (quarterTimeKey qt)
      putWord8 (quarterTimeValue qt)
    MidiMsgSongPosition (Position p) -> do
      putWord8 0xF2
      putWord8 (fromIntegral p .|. 0x7)
      putWord8 (fromIntegral (shiftR p 7))
    MidiMsgSongSelect (Song s) -> do
      putWord8 0xF3
      putWord8 s
    MidiMsgTuneRequest -> putWord8 0xF6
    MidiMsgSrtClock -> putWord8 0xF8
    MidiMsgSrtStart -> putWord8 0xFA
    MidiMsgSrtContinue -> putWord8 0xFB
    MidiMsgSrtStop -> putWord8 0xFC
    MidiMsgActiveSensing -> putWord8 0xFE
    MidiMsgReset -> putWord8 0xFF

newtype VarInt = VarInt { unVarInt :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq)

instance ByteSized VarInt where
  byteSize (VarInt w) =
    if
      | w .&. 0xFFFFFF80 == 0 -> 1
      | w .&. 0xFFFFC000 == 0 -> 2
      | w .&. 0xFFE00000 == 0 -> 3
      | otherwise -> 4

instance Binary VarInt where
  get = go 0 0 where
    go !off !acc = do
      w <- get @Word8
      let !wLow = fromIntegral (w .&. 0x7F)
          !wShift = shiftL wLow off
          !accNext = acc .|. wShift
      if w .&. 0x80 == 0
        then pure $! VarInt accNext
        else go (off + 7) accNext

  put (VarInt acc) = go acc where
    go !w = do
      let !wLow = fromIntegral (w .&. 0x7F)
          !wShift = shiftR w 7
      putWord8 wLow
      unless (wShift == 0) (go wShift)

-- | NOTE: Time delta is in number of ticks since previous message
data MidiEvent = MidiEvent
  { meTimeDelta :: !VarInt
  , meMessage :: !MidiMsg
  } deriving stock (Eq, Show, Generic)
    deriving (ByteSized, Binary) via (ViaGeneric MidiEvent)

type MidiTrackMagic = ExactBytes "MTrk"

newtype MidiTrack = MidiTrack { unMidiTrack :: Seq MidiEvent }
  deriving stock (Show)
  deriving newtype (Eq)

instance ByteSized MidiTrack where
  byteSize (MidiTrack events) = 6 + byteSizeFoldable events

instance Binary MidiTrack where
  get = do
    _ <- get @MidiTrackMagic
    Word16BE numEvents <- get
    events <- getSeq (fromIntegral numEvents) get
    pure $! MidiTrack events

  put (MidiTrack events) = do
    put @MidiTrackMagic (ExactBytes ())
    put (Word16BE (fromIntegral (Seq.length events)))
    putSeq put events

data MidiFileType =
    MidiFileTypeSingle
  | MidiFileTypeMultiSync
  | MidiFileTypeMultiAsync
  deriving stock (Eq, Ord, Enum, Bounded, Show)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaBinaryRep MidiFileType)

instance BinaryRep Word16BE MidiFileType where
  fromBinaryRep = \case
    0 -> Right MidiFileTypeSingle
    1 -> Right MidiFileTypeMultiSync
    2 -> Right MidiFileTypeMultiAsync
    other -> Left ("invalid midi file type: " ++ show other)
  toBinaryRep = \case
    MidiFileTypeSingle -> 0
    MidiFileTypeMultiSync -> 1
    MidiFileTypeMultiAsync -> 2

type MidiFileMagic = ExactBytes "MThd\NUL\NUL\NUL\ACK"

-- | NOTE: Ticks could also be SMTPE-related, but we don't support that here
data MidiFile = MidiFile
  { mfType :: !MidiFileType
  , mfTicks :: !Word16
  , mfTracks :: !(Seq MidiTrack)
  } deriving stock (Eq, Show, Generic)

instance ByteSized MidiFile where
  byteSize (MidiFile _ _ tracks) = 14 + byteSizeFoldable tracks

instance Binary MidiFile where
  get = do
    _ <- get @MidiFileMagic
    ty <- get
    Word16BE ticks <- get
    Word16BE numTracks <- get
    tracks <- getSeq (fromIntegral numTracks) get
    pure $! MidiFile ty ticks tracks
  put (MidiFile ty ticks tracks) = do
    put @MidiFileMagic (ExactBytes ())
    put ty
    put (Word16BE ticks)
    put (Word16BE (fromIntegral (Seq.length tracks)))
    putSeq put tracks
