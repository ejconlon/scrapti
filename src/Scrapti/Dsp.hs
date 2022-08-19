{-# LANGUAGE RecordWildCards #-}
module Scrapti.Dsp
  ( DspErr (..)
  , monoFromLeft
  , monoFromRight
  , monoFromAvg
  , ensureMonoFromLeft
  , stereoFromMono
  , linearCrossFade
  , crop
  , Mod (..)
  , modId
  , modAndThen
  , PcmMeta (..)
  , PcmContainer (..)
  , applyMod
  , applyModGeneric
  ) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Dahdit (Int16LE, Int24LE, Int32LE, Int8, LiftedPrim (..), LiftedPrimArray (LiftedPrimArray),
               cloneLiftedPrimArray, generateLiftedPrimArray, indexLiftedPrimArray, proxyForF, sizeofLiftedPrimArray)
import Data.Bits (Bits (..))
import Data.Primitive.ByteArray (ByteArray, sizeofByteArray)
import Data.Proxy (Proxy (..))

data Sampled f where
  Sampled :: (LiftedPrim a, Integral a) => !(f a) -> Sampled f

getSampled :: Int -> Maybe (Sampled Proxy)
getSampled = \case
  8 -> Just (Sampled (Proxy :: Proxy Int8))
  16 -> Just (Sampled (Proxy :: Proxy Int16LE))
  24 -> Just (Sampled (Proxy :: Proxy Int24LE))
  32 -> Just (Sampled (Proxy :: Proxy Int32LE))
  _ -> Nothing


data DspErr =
    DspErrOddSamples
  | DspErrBadElemSize
  | DspErrBadBitWidth
  | DspErrNotStereo
  | DspErrNotMono
  | DspErrBadFade
  | DspErrBadCrop
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

guardFade :: Int -> Int -> Int -> Either DspErr ()
guardFade width loopStart loopEnd = do
  if loopEnd <= loopStart ||
     loopStart <= loopStart - width ||
     loopStart + width <= loopStart ||
     loopEnd - width <= loopStart + width
    then Left DspErrBadFade
    else Right ()

combine :: Integral a => Int -> Int -> a -> a -> a
combine off width one two =
  let dist1 = fromIntegral off
      dist2 = fromIntegral (width - off)
      distTot = fromIntegral width
  in fromInteger (div (dist1 * fromIntegral one + dist2 * fromIntegral two) distTot)

-- Cross fade:  | --------- PreStart Start PostStart PreEnd End ---- |
-- Guarded to ensure inequalities are strict
linearCrossFade :: (LiftedPrim a, Integral a) => Int -> Int -> Int -> Mod a a
linearCrossFade width loopStart loopEnd = Mod $ \mm src -> do
  guardFade width loopStart loopEnd
  let !nc = mmNumChannels mm
      !sampWidth = nc * width
      !sampBetween = nc * (loopEnd - loopStart)
      !sampPreStart = nc * (loopStart - width)
      !sampPostStart = nc * (loopStart + width)
      !sampPreEnd = nc * loopEnd
      !sampEnd = nc * loopEnd
      genElem i =
        let !v = indexLiftedPrimArray src i
        in if
          | i >= sampPreStart && i <= sampPostStart ->
            let !w = indexLiftedPrimArray src (i + sampBetween)
                !dist = i - sampPreStart
            in combine dist sampWidth v w
          | i >= sampPreEnd && i <= sampEnd ->
            let !w = indexLiftedPrimArray src (i - sampBetween)
                !dist = i - sampPreEnd
            in combine dist sampWidth w v
          | otherwise -> v
      !sz = sizeofLiftedPrimArray src
      !dest = generateLiftedPrimArray sz genElem
  Right (mm, dest)

guardCrop :: Int -> Int -> Either DspErr ()
guardCrop start end = do
  if end <= start
    then Left DspErrBadCrop
    else Right ()

crop :: LiftedPrim a => Int -> Int -> Mod a a
crop start end = Mod $ \mm src -> do
  guardCrop start end
  let !nc = mmNumChannels mm
      !sampStart = nc * start
      !sampEnd = nc * end
      !dest = cloneLiftedPrimArray src sampStart sampEnd
  Right (mm, dest)

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

proxMod :: (LiftedPrim a, Integral a) => Proxy a -> (forall x. (LiftedPrim x, Integral x) => Mod x x) -> Mod a a
proxMod _ allMod = allMod

applyModGeneric :: (forall a. (LiftedPrim a, Integral a) => Mod a a) -> PcmContainer -> Either DspErr PcmContainer
applyModGeneric allMod con =
  case getSampled (div (pmBitsPerSample (pcMeta con)) 8) of
    Nothing -> Left DspErrBadBitWidth
    Just (Sampled prox) -> applyMod (proxMod prox allMod) con
