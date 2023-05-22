{-# LANGUAGE RecordWildCards #-}

module Scrapti.Dsp
  ( SampleCount (..)
  , DspErr (..)
  , monoFromLeft
  , monoFromRight
  , monoFromAvg
  , ensureMonoFromLeft
  , reduceBitDepth
  , stereoFromMono
  , linearCrossFade
  , crop
  , ModMeta (..)
  , Mod (..)
  , modId
  , modAndThen
  , PcmMeta (..)
  , PcmContainer (..)
  , applyMod
  , applyModGeneric
  )
where

import Control.Exception (Exception)
import Control.Monad (unless)
import Dahdit
  ( ByteCount (..)
  , ElemCount (..)
  , Int16LE
  , Int24LE
  , Int32LE
  , Int8
  , LiftedPrim (..)
  , LiftedPrimArray (..)
  , cloneLiftedPrimArray
  , generateLiftedPrimArray
  , indexLiftedPrimArray
  , lengthLiftedPrimArray
  , proxyForF
  , sizeofLiftedPrimArray
  , staticByteSize
  )
import Data.Bits (Bits (..))
import Data.Coerce (coerce)
import Data.Primitive.ByteArray (sizeofByteArray)
import Data.Proxy (Proxy (..))
import Scrapti.Binary (QuietArray (..))

newtype SampleCount = SampleCount {unSampleCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)

data Sampled f where
  Sampled :: (LiftedPrim a, Integral a) => !(f a) -> Sampled f

getSampled :: Int -> Maybe (Sampled Proxy)
getSampled = \case
  8 -> Just (Sampled (Proxy :: Proxy Int8))
  16 -> Just (Sampled (Proxy :: Proxy Int16LE))
  24 -> Just (Sampled (Proxy :: Proxy Int24LE))
  32 -> Just (Sampled (Proxy :: Proxy Int32LE))
  _ -> Nothing

data DspErr
  = DspErrOddSamples
  | DspErrBadElemSize
  | DspErrBadBitWidth
  | DspErrNotStereo
  | DspErrNotMono
  | DspErrBadFade
  | DspErrBadCrop
  deriving stock (Eq, Show)

instance Exception DspErr

newtype Sel a = Sel {runSel :: LiftedPrimArray a -> ElemCount -> a}

selMonoLeft, selMonoRight :: LiftedPrim a => Sel a
selMonoLeft = Sel $ \arr i -> indexLiftedPrimArray arr (i * 2)
selMonoRight = Sel $ \arr i -> indexLiftedPrimArray arr (i * 2 + 1)

selMonoAvg :: (LiftedPrim a, Integral a, Bits a) => Sel a
selMonoAvg = Sel $ \arr i ->
  let !ix = i * 2
      !lval = indexLiftedPrimArray arr ix
      !rval = indexLiftedPrimArray arr (ix + 1)
      !halfLval = div lval 2
      !halfRval = div rval 2
      !extra = lval .&. rval .&. 1
  in  halfLval + halfRval + extra

data ModMeta = ModMeta
  { mmNumChannels :: !Int
  , mmBitsPerSample :: !Int
  , mmSampleRate :: !Int
  }
  deriving stock (Eq, Show)

-- Array layout: samples are interspersed: [chan1 samp1, chan2 samp1, chan1 samp2, chan2 samp2, chan1 samp3, ...]
-- numChannels -> array -> (newNumChannels, newArray)
newtype Mod a b = Mod {runMod :: ModMeta -> LiftedPrimArray a -> Either DspErr (ModMeta, LiftedPrimArray b)}

modId :: Mod a a
modId = Mod (curry Right)

modAndThen :: Mod a b -> Mod b c -> Mod a c
modAndThen modAB modBC = Mod $ \nc src -> do
  (nc', src') <- runMod modAB nc src
  runMod modBC nc' src'

monoFromSel :: LiftedPrim a => Sel a -> Mod a a
monoFromSel sel = Mod $ \mm src -> do
  unless (mmNumChannels mm == 2) (Left DspErrNotStereo)
  let !srcLen = lengthLiftedPrimArray src
  unless (even srcLen) (Left DspErrOddSamples)
  let !destLen = div srcLen 2
      !dest = generateLiftedPrimArray destLen (runSel sel src)
  Right (mm {mmNumChannels = 1}, dest)

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

-- NOTE only implemented 16->16 (noop) and 24->16 conversion - will fail otherwise
reduceBitDepth :: Mod Int24LE Int16LE
reduceBitDepth = Mod $ \mm src -> do
  let !mm' = mm {mmBitsPerSample = 16}
  let !src24 = LiftedPrimArray (unLiftedPrimArray src) :: LiftedPrimArray Int24LE
  let go i =
        let x = indexLiftedPrimArray src24 i :: Int24LE
            y = shiftR (toInteger x) 8
        in  fromInteger y :: Int16LE
  let !dest16 = generateLiftedPrimArray (lengthLiftedPrimArray src24) go :: LiftedPrimArray Int16LE
  unless (2 * sizeofLiftedPrimArray src24 == 3 * sizeofLiftedPrimArray dest16) (error "Bad size (in bytes)")
  let !dest = LiftedPrimArray (unLiftedPrimArray dest16)
  pure (mm', dest)

stereoFromMono :: LiftedPrim a => Mod a a
stereoFromMono = Mod $ \mm src -> do
  unless (mmNumChannels mm == 1) (Left DspErrNotMono)
  let !srcLen = lengthLiftedPrimArray src
      !destLen = srcLen * 2
      !dest = generateLiftedPrimArray destLen (\i -> indexLiftedPrimArray src (div i 2))
  Right (mm {mmNumChannels = 2}, dest)

guardFade :: SampleCount -> SampleCount -> SampleCount -> Either DspErr ()
guardFade width loopStart loopEnd = do
  if width <= 0
    || loopEnd <= loopStart
    || loopStart <= loopStart - width
    || loopStart + width <= loopStart
    || loopEnd <= loopEnd - width
    || loopEnd + width <= loopEnd
    then Left DspErrBadFade
    else Right ()

combine :: Integral a => Int -> Int -> a -> a -> a
combine intDistTot intDist1 one two =
  let dist1 = fromIntegral intDist1
      dist2 = fromIntegral (intDistTot - intDist1)
      distTot = fromIntegral intDistTot
  in  fromInteger (div (dist1 * fromIntegral one + dist2 * fromIntegral two) distTot)

-- Cross fade:  | --------- PreStart Start PostStart PreEnd End PostEnd ---- |
-- Guarded to ensure inequalities are strict
-- Width here is one-sided
-- Width is number of samples to fade over
linearCrossFade :: (LiftedPrim a, Integral a) => SampleCount -> SampleCount -> SampleCount -> Mod a a
linearCrossFade width loopStart loopEnd = Mod $ \mm src -> do
  guardFade width loopStart loopEnd
  let !nc = mmNumChannels mm
      !sampWidth = nc * coerce width
      !sampTotDist = 2 * sampWidth
      !sampBetween = ElemCount (nc * coerce (loopEnd - loopStart))
      !sampPreStart = ElemCount (nc * coerce (loopStart - width))
      !sampStart = ElemCount (nc * coerce loopStart)
      !sampPostStart = ElemCount (nc * coerce (loopStart + width))
      !sampPreEnd = ElemCount (nc * coerce (loopEnd - width))
      !sampEnd = ElemCount (nc * coerce loopStart)
      !sampPostEnd = ElemCount (nc * coerce (loopEnd + width))
      !sz = lengthLiftedPrimArray src
      !sampLast = sz - sampBetween
      genElem i =
        let !v = indexLiftedPrimArray src i
        in  if
                | i >= sampPreStart && i <= sampPostStart ->
                    if i >= sampLast
                      then v
                      else
                        let !w = indexLiftedPrimArray src (i + sampBetween)
                            f = combine sampTotDist (coerce (sampPostStart - i))
                        in  if i < sampStart then f v w else f w v
                | i >= sampPreEnd && i <= sampPostEnd ->
                    if i < sampBetween
                      then v
                      else
                        let !w = indexLiftedPrimArray src (i - sampBetween)
                            f = combine sampTotDist (coerce (sampPostEnd - i))
                        in  if i < sampEnd then f v w else f w v
                | otherwise -> v
      !dest = generateLiftedPrimArray sz genElem
  Right (mm, dest)

guardCrop :: SampleCount -> SampleCount -> Either DspErr ()
guardCrop start end = do
  if end <= start
    then Left DspErrBadCrop
    else Right ()

crop :: LiftedPrim a => SampleCount -> SampleCount -> Mod a a
crop start end = Mod $ \mm src -> do
  guardCrop start end
  let !nc = mmNumChannels mm
      !sampStart = coerce nc * coerce start
      !sampEnd = coerce nc * coerce end
      !dest = cloneLiftedPrimArray src sampStart (sampEnd - sampStart)
  Right (mm, dest)

data PcmMeta = PcmMeta
  { pmNumChannels :: !Int
  , pmNumSamples :: !SampleCount
  , pmBitsPerSample :: !Int
  , pmSampleRate :: !Int
  }
  deriving stock (Eq, Show)

data PcmContainer = PcmContainer
  { pcMeta :: !PcmMeta
  , pcData :: !QuietArray
  }
  deriving stock (Eq, Show)

pmToMm :: PcmMeta -> ModMeta
pmToMm (PcmMeta {..}) = ModMeta {mmNumChannels = pmNumChannels, mmBitsPerSample = pmBitsPerSample, mmSampleRate = pmSampleRate}

toLifted :: LiftedPrim a => Proxy a -> PcmContainer -> Either DspErr (ModMeta, LiftedPrimArray a)
toLifted prox (PcmContainer pm (QuietArray arr)) = do
  let !elemSize = staticByteSize prox
  let !actualNs = coerce (div (sizeofByteArray arr) (coerce elemSize * pmNumChannels pm))
  unless (coerce elemSize * 8 == pmBitsPerSample pm && actualNs == pmNumSamples pm) (Left DspErrBadElemSize)
  let !mm = pmToMm pm
  let !larr = LiftedPrimArray arr
  Right (mm, larr)

fromLifted :: LiftedPrim b => ModMeta -> LiftedPrimArray b -> Either DspErr PcmContainer
fromLifted mm larr@(LiftedPrimArray arr) = do
  let !elemSize = staticByteSize (proxyForF larr)
      !nc = mmNumChannels mm
      !ns = coerce (div (sizeofByteArray arr) (coerce elemSize * nc))
      !bps = coerce elemSize * 8
      !sr = mmSampleRate mm
      !extraElems = rem (sizeofByteArray arr) (coerce elemSize * nc)
  unless (extraElems == 0) (Left DspErrBadElemSize)
  let !pm = PcmMeta nc ns bps sr
  Right $! PcmContainer pm (QuietArray arr)

proxyFromFirst :: m a b -> Proxy a
proxyFromFirst _ = Proxy

applyMod :: (LiftedPrim a, LiftedPrim b) => Mod a b -> PcmContainer -> Either DspErr PcmContainer
applyMod modx con = do
  (mm, src) <- toLifted (proxyFromFirst modx) con
  (mm', dest) <- runMod modx mm src
  fromLifted mm' dest

proxMod :: (LiftedPrim a, Integral a) => Proxy a -> (forall x. (LiftedPrim x, Integral x) => Mod x x) -> Mod a a
proxMod _ allMod = allMod

applyModGeneric :: (forall a. (LiftedPrim a, Integral a) => Mod a a) -> PcmContainer -> Either DspErr PcmContainer
applyModGeneric allMod con =
  case getSampled (pmBitsPerSample (pcMeta con)) of
    Nothing -> Left DspErrBadBitWidth
    Just (Sampled prox) -> applyMod (proxMod prox allMod) con
