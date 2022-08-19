module Scrapti.Dsp
  ( DspErr (..)
  , monoFromLeft
  , monoFromRight
  , monoFromAvg
  , ensureMonoFromLeft
  , stereoFromMono
  , Mod (..)
  , modId
  , modAndThen
  , PcmMeta (..)
  , PcmContainer (..)
  , applyMod
  ) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Dahdit (LiftedPrim (..), LiftedPrimArray (LiftedPrimArray), generateLiftedPrimArray, indexLiftedPrimArray,
               sizeofLiftedPrimArray)
import Data.Bits (Bits (..))
import Data.Primitive.ByteArray (ByteArray, sizeofByteArray)
import Data.Proxy (Proxy (..))

data DspErr =
    DspErrOddSamples
  | DspErrBadElemSize
  | DspErrNotStereo
  | DspErrNotMono
  deriving stock (Eq, Show)

instance Exception DspErr

newtype Sel a = Sel { runSel :: LiftedPrimArray a -> Int -> a }

selMonoLeft, selMonoRight :: LiftedPrim a => Sel a
selMonoLeft = Sel $ \arr i -> indexLiftedPrimArray arr (div i 2)
selMonoRight = Sel $ \arr i -> indexLiftedPrimArray arr (div i 2 + 1)

selMonoAvg :: (LiftedPrim a, Integral a, Bits a) => Sel a
selMonoAvg = Sel $ \arr i ->
  let !ix = div i 2
      !lval = indexLiftedPrimArray arr ix
      !rval = indexLiftedPrimArray arr (ix + 1)
      !halfLval = div lval 2
      !halfRval = div rval 2
      !extra = lval .&. rval .&. 1
  in halfLval + halfRval + extra

-- Array layou: samples are interspersed: [chan1 samp1, chan2 samp1, chan1 samp2, chan2 samp2, chan1 samp3, ...]
-- numChannels -> array -> (newNumChannels, newArray)
newtype Mod a b = Mod { runMod :: Int -> LiftedPrimArray a -> Either DspErr (Int, LiftedPrimArray b) }

modId :: Mod a a
modId = Mod (curry Right)

modAndThen :: Mod a b -> Mod b c -> Mod a c
modAndThen modAB modBC = Mod $ \nc src -> do
  (nc', src') <- runMod modAB nc src
  runMod modBC nc' src'

monoFromSel :: LiftedPrim a => Sel a -> Mod a a
monoFromSel sel = Mod $ \nc src -> do
  unless (nc == 2) (Left DspErrNotStereo)
  let !srcLen = sizeofLiftedPrimArray src
  unless (even srcLen) (Left DspErrOddSamples)
  let !destLen = div srcLen 2
      !dest = generateLiftedPrimArray destLen (runSel sel src)
  Right (1, dest)

monoFromLeft, monoFromRight :: LiftedPrim a => Mod a a
monoFromLeft = monoFromSel selMonoLeft
monoFromRight = monoFromSel selMonoRight

monoFromAvg :: (LiftedPrim a, Integral a, Bits a) => Mod a a
monoFromAvg = monoFromSel selMonoAvg

ensureMonoFromSel :: LiftedPrim a => Sel a -> Mod a a
ensureMonoFromSel sel = Mod $ \nc src -> do
  if nc == 1
    then pure (nc, src)
    else runMod (monoFromSel sel) nc src

ensureMonoFromLeft :: LiftedPrim a => Mod a a
ensureMonoFromLeft = ensureMonoFromSel selMonoLeft

stereoFromMono :: LiftedPrim a => Mod a a
stereoFromMono = Mod $ \nc src -> do
  unless (nc == 1) (Left DspErrNotMono)
  let !srcLen = sizeofLiftedPrimArray src
      !destLen = srcLen * 2
      !dest = generateLiftedPrimArray destLen (\i -> indexLiftedPrimArray src (div i 2))
  Right (2, dest)

data PcmMeta = PcmMeta
  { pmNumChannels :: !Int
  , pmNumSamples :: !Int
  , pmBitsPerSample :: !Int
  } deriving stock (Eq, Show)

data PcmContainer = PcmContainer
  { pcMeta :: !PcmMeta
  , pcData :: !ByteArray
  } deriving stock (Eq, Show)

proxyFromFirst :: m a b -> Proxy a
proxyFromFirst _ = Proxy

proxyFromSecond :: m a b -> Proxy b
proxyFromSecond _ = Proxy

toLifted :: LiftedPrim a => Proxy a -> PcmContainer -> Either DspErr (LiftedPrimArray a)
toLifted prox (PcmContainer (PcmMeta nc ns bps) arr) = do
  let !elemSize = elemSizeLifted prox
  let !actualNs = div (sizeofByteArray arr) (elemSize * nc)
  unless (elemSize * 8 == bps && actualNs == ns) (Left DspErrBadElemSize)
  Right $! LiftedPrimArray arr

fromLifted :: LiftedPrim b => Proxy b -> Int -> LiftedPrimArray b -> Either DspErr PcmContainer
fromLifted prox nc (LiftedPrimArray arr) = do
  let !elemSize = elemSizeLifted prox
      !ns = div (sizeofByteArray arr) (elemSize * nc)
      !bps = elemSize * 8
      !extraElems = rem (sizeofByteArray arr) (elemSize * nc)
  unless (extraElems == 0) (Left DspErrBadElemSize)
  Right $! PcmContainer (PcmMeta nc ns bps) arr

applyMod :: (LiftedPrim a, LiftedPrim b) => Mod a b -> PcmContainer -> Either DspErr PcmContainer
applyMod modx con = do
  let nc = pmNumChannels (pcMeta con)
  src <- toLifted (proxyFromFirst modx) con
  (nc', dest) <- runMod modx nc src
  fromLifted (proxyFromSecond modx) nc' dest
