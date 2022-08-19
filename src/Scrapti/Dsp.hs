{-# LANGUAGE RecordWildCards #-}
module Scrapti.Dsp
  ( DspErr (..)
  , monoFromLeft
  , monoFromRight
  , monoFromAvg
  , ensureMonoFromLeft
  , stereoFromMono
  , monoLinearCrossFade
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
               proxyForF, sizeofLiftedPrimArray)
import Data.Bits (Bits (..))
import Data.Primitive.ByteArray (ByteArray, sizeofByteArray)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)

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

data ModMeta = ModMeta
  { mmNumChannels :: !Int
  , mmBitsPerSample :: !Int
  , mmSampleRate :: !Int
  } deriving stock (Eq, Show)

-- Array layout: samples are interspersed: [chan1 samp1, chan2 samp1, chan1 samp2, chan2 samp2, chan1 samp3, ...]
-- numChannels -> array -> (newNumChannels, newArray)
newtype Mod a b = Mod { runMod :: ModMeta -> LiftedPrimArray a -> Either DspErr (ModMeta, LiftedPrimArray b) }

modId :: Mod a a
modId = Mod (curry Right)

modAndThen :: Mod a b -> Mod b c -> Mod a c
modAndThen modAB modBC = Mod $ \nc src -> do
  (nc', src') <- runMod modAB nc src
  runMod modBC nc' src'

monoFromSel :: LiftedPrim a => Sel a -> Mod a a
monoFromSel sel = Mod $ \mm src -> do
  unless (mmNumChannels mm == 2) (Left DspErrNotStereo)
  let !srcLen = sizeofLiftedPrimArray src
  unless (even srcLen) (Left DspErrOddSamples)
  let !destLen = div srcLen 2
      !dest = generateLiftedPrimArray destLen (runSel sel src)
  Right (mm { mmNumChannels = 1 }, dest)

monoFromLeft, monoFromRight :: LiftedPrim a => Mod a a
monoFromLeft = monoFromSel selMonoLeft
monoFromRight = monoFromSel selMonoRight

monoFromAvg :: (LiftedPrim a, Integral a, Bits a) => Mod a a
monoFromAvg = monoFromSel selMonoAvg

ensureMonoFromSel :: LiftedPrim a => Sel a -> Mod a a
ensureMonoFromSel sel = Mod $ \mm src -> do
  if mmNumChannels mm == 1
    then pure (mm, src)
    else runMod (monoFromSel sel) mm src

ensureMonoFromLeft :: LiftedPrim a => Mod a a
ensureMonoFromLeft = ensureMonoFromSel selMonoLeft

stereoFromMono :: LiftedPrim a => Mod a a
stereoFromMono = Mod $ \mm src -> do
  unless (mmNumChannels mm == 1) (Left DspErrNotMono)
  let !srcLen = sizeofLiftedPrimArray src
      !destLen = srcLen * 2
      !dest = generateLiftedPrimArray destLen (\i -> indexLiftedPrimArray src (div i 2))
  Right (mm { mmNumChannels = 2 }, dest)

monoLinearCrossFade :: (LiftedPrim a, Integral a) => Word32 -> Word32 -> Mod a a
monoLinearCrossFade loopStart loopEnd = Mod $ \mm src -> do
  unless (mmNumChannels mm == 1) (Left DspErrNotMono)
  -- TODO
  Right (mm, src)

monoCrop :: LiftedPrim a => Word32 -> Word32 -> Mod a a
monoCrop start end = Mod $ \mm src -> do
  unless (mmNumChannels mm == 1) (Left DspErrNotMono)
  -- TODO
  Right (mm, src)

data PcmMeta = PcmMeta
  { pmNumChannels :: !Int
  , pmNumSamples :: !Int
  , pmBitsPerSample :: !Int
  , pmSampleRate :: !Int
  } deriving stock (Eq, Show)

data PcmContainer = PcmContainer
  { pcMeta :: !PcmMeta
  , pcData :: !ByteArray
  } deriving stock (Eq, Show)

pmToMm :: PcmMeta -> ModMeta
pmToMm (PcmMeta {..}) = ModMeta { mmNumChannels = pmNumChannels, mmBitsPerSample = pmBitsPerSample, mmSampleRate = pmSampleRate }

toLifted :: LiftedPrim a => Proxy a -> PcmContainer -> Either DspErr (ModMeta, LiftedPrimArray a)
toLifted prox (PcmContainer pm arr) = do
  let !elemSize = elemSizeLifted prox
  let !actualNs = div (sizeofByteArray arr) (elemSize * pmNumChannels pm)
  unless (elemSize * 8 == pmBitsPerSample pm && actualNs == pmNumSamples pm) (Left DspErrBadElemSize)
  let !mm = pmToMm pm
  let !larr = LiftedPrimArray arr
  Right (mm, larr)

fromLifted :: LiftedPrim b => ModMeta -> LiftedPrimArray b -> Either DspErr PcmContainer
fromLifted mm larr@(LiftedPrimArray arr) = do
  let !elemSize = elemSizeLifted (proxyForF larr)
      !nc = mmNumChannels mm
      !ns = div (sizeofByteArray arr) (elemSize * nc)
      !bps = elemSize * 8
      !sr = mmSampleRate mm
      !extraElems = rem (sizeofByteArray arr) (elemSize * nc)
  unless (extraElems == 0) (Left DspErrBadElemSize)
  let !pm = PcmMeta nc ns bps sr
  Right $! PcmContainer pm arr

proxyFromFirst :: m a b -> Proxy a
proxyFromFirst _ = Proxy

applyMod :: (LiftedPrim a, LiftedPrim b) => Mod a b -> PcmContainer -> Either DspErr PcmContainer
applyMod modx con = do
  (mm, src) <- toLifted (proxyFromFirst modx) con
  (mm', dest) <- runMod modx mm src
  fromLifted mm' dest
