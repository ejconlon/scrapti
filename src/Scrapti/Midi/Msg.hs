module Scrapti.Midi.Msg
  ( Note (..)
  , Velocity (..)
  )
where

import Data.Word (Word8)

newtype Note = Note {unNote :: Word8}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype Velocity = Velocity {unVelocity :: Word8}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)
