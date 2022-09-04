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
  -- , noteOn
  -- , noteOff
  -- , ChanVoiceData (..)
  , ChanStatus (..)
  , CommonStatus (..)
  , RtStatus (..)
  , MidiStatus (..)
  , MidiMsg (..)
  , MidiEvent (..)
  , MidiTrack (..)
  , MidiFile (..)
  ) where

import Control.Monad (unless)
import Dahdit (Binary (..), BinaryRep (..), ByteCount, ByteSized (..), ExactBytes (..), Get, StaticByteSized (..),
               ViaBinaryRep (..), ViaStaticByteSized (..), Word16BE (..), byteSizeFoldable,
               getByteString, getLookAhead, getSeq, getWord8, putByteString, putSeq, putWord8, PutM, Put)
import Data.Bits (Bits (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Int (Int16)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Word (Word16, Word32, Word8)
import GHC.Generics (Generic)

newtype Channel = Channel { unChannel :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype ChannelCount = ChannelCount { unChannelCount :: Word8 }
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

-- noteOn :: Channel -> Note -> Velocity -> MidiMsg
-- noteOn c k v = MidiMsgChanVoice (ChanVoiceMsg c (ChanVoiceNoteOnOff k v))

-- noteOff :: Channel -> Note -> MidiMsg
-- noteOff c k = noteOn c k 0

newtype SysExString = SysExString { unSysExString :: ShortByteString }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance ByteSized SysExString where
  byteSize = fromIntegral . BSS.length . unSysExString

findDataLength :: Get ByteCount
findDataLength = getLookAhead (go 0) where
  go !i = do
    w <- getWord8
    if w .&. 0x80 == 0
      then go (i + 1)
      else pure i

instance Binary SysExString where
  get = fmap SysExString (findDataLength >>= getByteString)
  put (SysExString ss) = putByteString ss

data ChanStatusType =
    ChanStatusNoteOff
  | ChanStatusNoteOn
  | ChanStatusKeyAftertouch
  | ChanStatusControlChange
  | ChanStatusProgramChange
  | ChanStatusChanAftertouch
  | ChanStatusPitchBend
  deriving stock (Eq, Show)

data CommonStatus =
    CommonStatusTimeFrame
  | CommonStatusSongPointer
  | CommonStatusSongSelect
  | CommonStatusTuneRequest
  | CommonStatusEndExclusive
  deriving stock (Eq, Show)

data RtStatus =
    RtStatusTimingClock
  | RtStatusStart
  | RtStatusContinue
  | RtStatusStop
  | RtStatusActiveSensing
  | RtStatusActiveSystemReset
  deriving stock (Eq, Show)

data ChanStatus = ChanStatus !Channel !ChanStatusType
  deriving stock (Eq, Show)

data MidiStatus =
    MidiStatusChan !ChanStatus
  | MidiStatusSysEx
  | MidiStatusSysCommon !CommonStatus
  | MidiStatusSysRt !RtStatus
  deriving stock (Eq, Show)
  deriving (ByteSized) via (ViaStaticByteSized MidiStatus)

instance StaticByteSized MidiStatus where
  staticByteSize _ = 1

midiStatusIsChan :: MidiStatus -> Bool
midiStatusIsChan = \case
  MidiStatusChan _ -> True
  _ -> False

midiStatusAsChan :: MidiStatus -> Maybe ChanStatus
midiStatusAsChan = \case
  MidiStatusChan cs -> Just cs
  _ -> Nothing

data StatusPeek =
    StatusPeekYes
  | StatusPeekNo !Word8
  deriving stock (Eq, Show)

peekStatus :: Get StatusPeek
peekStatus = getLookAhead $ do
  b <- getWord8
  pure $! if b .&. 0x80 == 0
    then StatusPeekNo b
    else StatusPeekYes

instance Binary MidiStatus where
  get = do
    b <- getWord8
    let !x = shiftL b 4
    if
      | x < 0x8 -> fail ("Midi status byte with high bit clear: " ++ show x)
      | x == 0xF -> error "TODO parse system msgs"
      | otherwise -> do
        let !d = b .&. 0xF
        pure $! MidiStatusChan $ ChanStatus (Channel d) $ case x of
          0x8 -> ChanStatusNoteOff
          0x9 -> ChanStatusNoteOn
          0xA -> ChanStatusKeyAftertouch
          0xB -> ChanStatusControlChange
          0xC -> ChanStatusProgramChange
          0xD -> ChanStatusChanAftertouch
          0xE -> ChanStatusPitchBend
          _ -> error "impossible"
  put = \case
    MidiStatusChan (ChanStatus (Channel c) cs) ->
      let !d = min 15 c
          !x = case cs of
            ChanStatusNoteOff -> 0x80
            ChanStatusNoteOn -> 0x90
            ChanStatusKeyAftertouch -> 0xA0
            ChanStatusControlChange -> 0xB0
            ChanStatusProgramChange -> 0xC0
            ChanStatusChanAftertouch -> 0xD0
            ChanStatusPitchBend -> 0xE0
      in putWord8 (d .|. x)
    MidiStatusSysEx -> putWord8 0xF0
    MidiStatusSysCommon cs ->
      let !x = case cs of
            CommonStatusTimeFrame -> 0x1
            CommonStatusSongPointer -> 0x2
            CommonStatusSongSelect -> 0x3
            CommonStatusTuneRequest -> 0x6
            CommonStatusEndExclusive -> 0x7
      in putWord8 (0xF0 .|. x)
    MidiStatusSysRt rs ->
      let !x = case rs of
            RtStatusTimingClock -> 0x0
            RtStatusStart -> 0x2
            RtStatusContinue -> 0x3
            RtStatusStop -> 0x4
            RtStatusActiveSensing -> 0x6
            RtStatusActiveSystemReset -> 0x7
      in putWord8 (0xF8 .|. x)

data ChanVoiceData =
    ChanVoiceDataNoteOff !Note !Velocity
  | ChanVoiceDataNodeOn !Note !Velocity
  | ChanVoiceKeyAftertouch !Note !Pressure
  | ChanVoiceControlChange !ControlNum !ControlVal
  | ChanVoiceProgramChange !ProgramNum
  | ChanVoiceChanAftertouch !Pressure
  | ChanVoicePitchBend !PitchBend
  deriving stock (Eq, Show)

instance ByteSized ChanVoiceData where
  byteSize = \case
    ChanVoiceDataNoteOff _ _ -> 2
    ChanVoiceDataNodeOn _ _ -> 2
    ChanVoiceKeyAftertouch _ _ -> 2
    ChanVoiceControlChange _ _ -> 2
    ChanVoiceProgramChange _ -> 1
    ChanVoiceChanAftertouch _ -> 1
    ChanVoicePitchBend _ -> 2

putChanVoiceData :: ChanVoiceData -> Put
putChanVoiceData = \case
  ChanVoiceDataNoteOff (Note n) (Velocity v) -> do
    putWord8 n
    putWord8 v
  ChanVoiceDataNodeOn (Note n) (Velocity v) -> do
    putWord8 n
    putWord8 v
  ChanVoiceKeyAftertouch (Note n) (Pressure p) -> do
    putWord8 n
    putWord8 p
  ChanVoiceControlChange (ControlNum cn) (ControlVal cv) -> do
    putWord8 cn
    putWord8 cv
  ChanVoiceProgramChange (ProgramNum pn) -> do
    putWord8 pn
  ChanVoiceChanAftertouch (Pressure p) -> do
    putWord8 p
  ChanVoicePitchBend (PitchBend pb) -> do
    let !w = min 16383 (max 0 (pb + 8192))
    putWord8 (fromIntegral w .|. 0x7)
    putWord8 (fromIntegral (shiftR w 7))

data ChanModeData =
    ChanModeAllSoundOff
  | ChanModeResetAllControllers
  | ChanModeLocalControlOff
  | ChanModeLocalControlOn
  | ChanModeAllNotesOff
  | ChanModeOmniOff
  | ChanModeOmniOn
  | ChanModeMonoOn !ChannelCount
  | ChanModeMonoOff
  deriving stock (Eq, Show)
  deriving (ByteSized) via (ViaStaticByteSized ChanModeData)

instance StaticByteSized ChanModeData where
  staticByteSize _ = 2

putChanModeData :: ChanModeData -> Put
putChanModeData = \case
  ChanModeAllSoundOff -> do
    putWord8 120
    putWord8 0
  ChanModeResetAllControllers -> do
    putWord8 121
    putWord8 0
  ChanModeLocalControlOff -> do
    putWord8 122
    putWord8 0
  ChanModeLocalControlOn -> do
    putWord8 122
    putWord8 127
  ChanModeAllNotesOff -> do
    putWord8 123
    putWord8 0
  ChanModeOmniOff -> do
    putWord8 124
    putWord8 0
  ChanModeOmniOn -> do
    putWord8 125
    putWord8 0
  ChanModeMonoOn (ChannelCount cc) -> do
    putWord8 126
    putWord8 cc
  ChanModeMonoOff -> do
    putWord8 127
    putWord8 0

data ChanData =
    ChanDataVoice !ChanVoiceData
  | ChanDataMode !ChanModeData
  deriving stock (Eq, Show)

getChanData :: ChanStatus -> Get ChanData
getChanData (ChanStatus _ ty) = case ty of
  ChanStatusNoteOff -> error "TODO"
  ChanStatusNoteOn -> error "TODO"
  ChanStatusKeyAftertouch -> error "TODO"
  ChanStatusControlChange -> error "TODO"
  ChanStatusProgramChange -> error "TODO"
  ChanStatusChanAftertouch -> error "TODO"
  ChanStatusPitchBend -> error "TODO"


data SysExData = SysExData
  { sedManf :: !Manf
  , sedBody :: !SysExString
  } deriving stock (Eq, Show)

instance ByteSized SysExData where
  byteSize (SysExData _ body) = 1 + byteSize body

getSysExData :: Get SysExData
getSysExData = error "TODO"

putSysExData :: SysExData -> Put
putSysExData = error "TODO"

data CommonData =
    CommonDataTimeFrame !QuarterTime
  | CommonDataSongPointer !Position
  | CommonDataSongSelect !Song
  | CommonDataTuneRequest
  | CommonDataEndExclusive
  deriving stock (Eq, Show)

instance ByteSized CommonData where
  byteSize = \case
    CommonDataTimeFrame _ -> 1
    CommonDataSongPointer _ -> 2
    CommonDataSongSelect _ -> 1
    CommonDataTuneRequest -> 0
    CommonDataEndExclusive -> 0

getCommonData :: CommonStatus -> Get CommonData
getCommonData = error "TODO"

putCommonData :: CommonData -> Put
putCommonData = \case
  CommonDataTimeFrame _qt -> error "TODO"
  CommonDataSongPointer _po -> error "TODO"
  CommonDataSongSelect _so -> error "TODO"
  CommonDataTuneRequest -> pure ()
  CommonDataEndExclusive -> pure ()

data RtData =
    RtDataTimingClock
  | RtDataStart
  | RtDataContinue
  | RtDataStop
  | RtDataActiveSensing
  | RtDataSystemReset
  deriving stock (Eq, Show)
  deriving (ByteSized) via (ViaStaticByteSized RtData)

instance StaticByteSized RtData where
  staticByteSize _ = 0

getRtData :: RtStatus -> Get RtData
getRtData = error "TODO"

putRtData :: RtData -> Put
putRtData _ = pure ()

data MidiData =
    MidiDataChanVoice !ChanVoiceData
  | MidiDataChanMode !ChanModeData
  | MidiDataSysEx !SysExData
  | MidiDataSysCommon !CommonData
  | MidiDataSysRt !RtData
  deriving stock (Eq, Show)

instance ByteSized MidiData where
  byteSize = \case
    MidiDataChanVoice cvd -> byteSize cvd
    MidiDataChanMode cmd -> byteSize cmd
    MidiDataSysEx sed -> byteSize sed
    MidiDataSysCommon cd -> byteSize cd
    MidiDataSysRt rd -> byteSize rd

getMidiMsg :: MidiStatus -> Get MidiMsg
getMidiMsg = error "TODO"

-- Running status is for Voice and Mode messages only!
getMidiDataRunning :: Maybe ChanStatus -> Get MidiMsg
getMidiDataRunning mayLastStatus = do
  peeked <- peekStatus
  case peeked of
    StatusPeekYes -> do
      status <- get @MidiStatus
      getMidiMsg status
    StatusPeekNo dat ->
      case mayLastStatus of
        Nothing -> fail ("Expected status byte (no running status): " ++ show dat)
        Just lastStatus -> getMidiMsg (MidiStatusChan lastStatus)

putMidiDataRunning :: Maybe ChanStatus -> MidiMsg -> PutM (Maybe ChanStatus)
putMidiDataRunning mayLastStatus = \case
  MidiMsgChanVoice cs cvd -> do
    unless (mayLastStatus == Just cs) (put (MidiStatusChan cs))
    putChanVoiceData cvd
    pure (Just cs)
  MidiMsgChanMode cs cmd -> do
    unless (mayLastStatus == Just cs) (put (MidiStatusChan cs))
    putChanModeData cmd
    pure (Just cs)
  MidiMsgSysEx sed -> do
    put MidiStatusSysEx
    putSysExData sed
    pure Nothing
  MidiMsgSysCommon cs cd -> do
    put (MidiStatusSysCommon cs)
    putCommonData cd
    pure Nothing
  MidiMsgSysRt rs rd -> do
    put (MidiStatusSysRt rs)
    putRtData rd
    pure Nothing

data MidiMsg =
    MidiMsgChanVoice !ChanStatus !ChanVoiceData
  | MidiMsgChanMode !ChanStatus !ChanModeData
  | MidiMsgSysEx !SysExData
  | MidiMsgSysCommon !CommonStatus !CommonData
  | MidiMsgSysRt !RtStatus !RtData
  deriving stock (Eq, Show)

midiMsgStatus :: MidiMsg -> MidiStatus
midiMsgStatus = \case
  MidiMsgChanVoice cs _ -> MidiStatusChan cs
  MidiMsgChanMode cs _ -> MidiStatusChan cs
  MidiMsgSysEx _ -> MidiStatusSysEx
  MidiMsgSysCommon cs _ -> MidiStatusSysCommon cs
  MidiMsgSysRt rs _ -> MidiStatusSysRt rs

midiMsgData :: MidiMsg -> MidiData
midiMsgData = \case
  MidiMsgChanVoice _ cvd -> MidiDataChanVoice cvd
  MidiMsgChanMode _ cmd -> MidiDataChanMode cmd
  MidiMsgSysEx sed -> MidiDataSysEx sed
  MidiMsgSysCommon _ cd -> MidiDataSysCommon cd
  MidiMsgSysRt _ rd -> MidiDataSysRt rd

--   put = \case
--     MidiMsgChanVoice (ChanVoiceMsg (Channel c) dat) ->
--       let !d = min 15 c
--       in case dat of
--         ChanVoiceNoteOnOff (Note n) (Velocity v) -> do
--           putWord8 (d .|. 0x90)
--           putWord8 n
--           putWord8 v
--         ChanVoicePolyAftertouch (Note n) (Pressure p) -> do
--           putWord8 (d .|. 0xA0)
--           putWord8 n
--           putWord8 p
--         ChanVoiceCC (ControlNum cn) (ControlVal cv) -> do
--           putWord8 (d .|. 0xB0)
--           putWord8 cn
--           putWord8 cv
--         ChanVoiceProgramChange (ProgramNum pn) -> do
--           putWord8 (d .|. 0xC0)
--           putWord8 pn
--         ChanVoiceAftertouch (Pressure p) -> do
--           putWord8 (d .|. 0xD0)
--           putWord8 p
--         ChanVoicePitchWheel (PitchBend pb) -> do
--           putWord8 (d .|. 0xE0)
--           let !w = min 16383 (max 0 (pb + 8192))
--           putWord8 (fromIntegral w .|. 0x7)
--           putWord8 (fromIntegral (shiftR w 7))
--     MidiMsgSysex (Manf m) ss -> do
--       putWord8 0xF0
--       putWord8 m
--       put ss
--     MidiMsgQuarterFrame qt -> do
--       putWord8 0xF1
--       putWord8 (quarterTimeKey qt)
--       putWord8 (quarterTimeValue qt)
--     MidiMsgSongPosition (Position p) -> do
--       putWord8 0xF2
--       putWord8 (fromIntegral p .|. 0x7)
--       putWord8 (fromIntegral (shiftR p 7))
--     MidiMsgSongSelect (Song s) -> do
--       putWord8 0xF3
--       putWord8 s
--     MidiMsgTuneRequest -> putWord8 0xF6
--     MidiMsgSrtClock -> putWord8 0xF8
--     MidiMsgSrtStart -> putWord8 0xFA
--     MidiMsgSrtContinue -> putWord8 0xFB
--     MidiMsgSrtStop -> putWord8 0xFC
--     MidiMsgActiveSensing -> putWord8 0xFE
--     MidiMsgReset -> putWord8 0xFF

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

type MidiTrackMagic = ExactBytes "MTrk"

newtype MidiTrack = MidiTrack { unMidiTrack :: Seq MidiEvent }
  deriving stock (Show)
  deriving newtype (Eq)

byteSizeEventsLoop :: ByteCount -> Maybe ChanStatus -> Seq MidiEvent -> ByteCount
byteSizeEventsLoop !bc !mayLastStatus = \case
  Empty -> bc
  MidiEvent td msg :<| mes ->
    let !tc = byteSize td
        !anyNextStatus = midiMsgStatus msg
        !mayNextStatus = midiStatusAsChan anyNextStatus
        !sc = case mayNextStatus of
          Just _ | mayNextStatus == mayLastStatus -> 0
          _ -> byteSize anyNextStatus
        !mc = byteSize (midiMsgData msg)
    in byteSizeEventsLoop (bc + tc + sc + mc) mayNextStatus mes

byteSizeEvents :: Seq MidiEvent -> ByteCount
byteSizeEvents = byteSizeEventsLoop 0 Nothing

instance ByteSized MidiTrack where
  byteSize (MidiTrack events) = 6 + byteSizeEvents events

getEventsLoop :: Int -> Seq MidiEvent -> Maybe ChanStatus -> Get (Seq MidiEvent)
getEventsLoop !numLeft !acc !mayLastStatus =
  if numLeft <= 0
    then pure acc
    else do
      td <- get
      msg <- getMidiDataRunning mayLastStatus
      let !me = MidiEvent td msg
          !mayNextStatus = midiStatusAsChan (midiMsgStatus msg)
      getEventsLoop (numLeft - 1) (acc :|> me) mayNextStatus

getEvents :: Int -> Get (Seq MidiEvent)
getEvents numLeft = getEventsLoop numLeft Empty Nothing

putEventsLoop :: Maybe ChanStatus -> Seq MidiEvent -> Put
putEventsLoop !mayLastStatus = \case
  Empty -> pure ()
  MidiEvent td msg :<| mes -> do
    put td
    mayNextStatus <- putMidiDataRunning mayLastStatus msg
    putEventsLoop mayNextStatus mes

putEvents :: Seq MidiEvent -> Put
putEvents = putEventsLoop Nothing

instance Binary MidiTrack where
  get = do
    _ <- get @MidiTrackMagic
    Word16BE numEvents <- get
    events <- getEvents (fromIntegral numEvents)
    pure $! MidiTrack events

  put (MidiTrack events) = do
    put @MidiTrackMagic (ExactBytes ())
    put (Word16BE (fromIntegral (Seq.length events)))
    putEvents events

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
