module Scrapti.Dsp
  ( DspErr (..)
  , monoFromLeft
  , monoFromRight
  , monoFromAvg
  , stereoFromMono
  , Mod (..)
  , PcmMeta (..)
  , PcmContainer (..)
  , PcmContainerLens
  , applyMod
  ) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Dahdit (LiftedPrim (..), LiftedPrimArray (LiftedPrimArray), generateLiftedPrimArray, indexLiftedPrimArray,
               sizeofLiftedPrimArray)
import Data.Bits (Bits (..))
import Data.Primitive.ByteArray (ByteArray, sizeofByteArray)
import Data.Proxy (Proxy (..))
import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)

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

type PcmContainerLens x = Lens' x PcmContainer

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

applyMod :: (LiftedPrim a, LiftedPrim b) => Mod a b -> PcmContainerLens x -> x -> Either DspErr x
applyMod modx lenz x = do
  let con = view lenz x
      nc = pmNumChannels (pcMeta con)
  src <- toLifted (proxyFromFirst modx) con
  (nc', dest) <- runMod modx nc src
  con' <- fromLifted (proxyFromSecond modx) nc' dest
  Right $! set lenz con' x
