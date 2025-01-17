module Scrapti.Midi.Time
  ( TimeDelta
  , timeDeltaFromFracSecs
  , timeDeltaFromNanos
  , timeDeltaToFracSecs
  , timeDeltaToNanos
  , diffTimeDelta
  , MonoTime
  , monoTimeToFracSecs
  , monoTimeToNanos
  , monoTimeFromFracSecs
  , monoTimeFromNanos
  , currentMonoTime
  , addMonoTime
  , diffMonoTime
  , threadDelayDelta
  , awaitDelta
  , assertingNonNegative
  )
where

import Control.Concurrent (threadDelay)
import Data.Functor (($>))
import Data.Semigroup (Sum (..))
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

assertingNonNegative :: (HasCallStack, Ord a, Num a, Show a) => a -> a
assertingNonNegative a =
  if a < 0
    then error ("Required non-negative value but got " ++ show a)
    else a

-- | Non-negative time difference in nanoseconds since last event
newtype TimeDelta = TimeDelta {unTimeDelta :: Word64}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Bounded, Num)
  deriving (Semigroup, Monoid) via (Sum Word64)

-- | Return a 'TimeDelta' corresponding the the given number of fractional seconds.
-- (For example, 1.5 represents one and a half seconds.)
timeDeltaFromFracSecs :: (Real a, Show a) => a -> TimeDelta
timeDeltaFromFracSecs d = TimeDelta (round (1000000000 * toRational (assertingNonNegative d)))

-- | Return a 'TimeDelta' corresponding the the given number of nanoseconds.
-- (For example, 1000000000 represends one second.)
timeDeltaFromNanos :: (Integral a, Show a) => a -> TimeDelta
timeDeltaFromNanos = TimeDelta . fromIntegral . assertingNonNegative

timeDeltaToFracSecs :: (Fractional a) => TimeDelta -> a
timeDeltaToFracSecs (TimeDelta n) = fromIntegral n / 1000000000

timeDeltaToNanos :: TimeDelta -> Word64
timeDeltaToNanos = unTimeDelta

-- | Return the difference of two time deltas
diffTimeDelta
  :: TimeDelta
  -- ^ the "larger" delta
  -> TimeDelta
  -- ^ the "smaller" delta
  -> Maybe TimeDelta
  -- ^ difference between the two (Nothing if negative)
diffTimeDelta (TimeDelta big) (TimeDelta small) =
  if big <= small
    then Nothing
    else Just (TimeDelta (big - small))

-- | Monotonic time in nanoseconds since some unspecified epoch (see 'getMonotonicTimeNs')
newtype MonoTime = MonoTime {unMonoTime :: Word64}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Bounded)

monoTimeFromFracSecs :: (Real a, Show a) => a -> MonoTime
monoTimeFromFracSecs d = MonoTime (round (1000000000 * toRational (assertingNonNegative d)))

monoTimeFromNanos :: (Integral a, Show a) => a -> MonoTime
monoTimeFromNanos = MonoTime . fromIntegral . assertingNonNegative

monoTimeToFracSecs :: (Fractional a) => MonoTime -> a
monoTimeToFracSecs (MonoTime n) = fromIntegral n / 1000000000

monoTimeToNanos :: MonoTime -> Word64
monoTimeToNanos = unMonoTime

currentMonoTime :: IO MonoTime
currentMonoTime = fmap MonoTime getMonotonicTimeNSec

addMonoTime :: MonoTime -> TimeDelta -> MonoTime
addMonoTime (MonoTime mt) (TimeDelta td) = MonoTime (mt + td)

diffMonoTime :: MonoTime -> MonoTime -> Maybe TimeDelta
diffMonoTime (MonoTime end) (MonoTime start) =
  if end <= start
    then Nothing
    else Just (TimeDelta (end - start))

threadDelayDelta :: TimeDelta -> IO ()
threadDelayDelta (TimeDelta td) = threadDelay (fromIntegral (div td 1000))

awaitDelta :: MonoTime -> TimeDelta -> IO MonoTime
awaitDelta m t = do
  let target = addMonoTime m t
  cur <- currentMonoTime
  case diffMonoTime target cur of
    Nothing -> pure cur
    Just td -> threadDelayDelta td $> target
