module Scrapti.Dsp
  ( DspErr (..)
  , Sel
  , selMonoLeft
  , selMonoRight
  , selMonoAvg
  , monoFromSel
  , monoFromLeft
  , monoFromRight
  , monoFromAvg
  , stereoFromMono
  ) where

import Control.Exception (Exception)
import Dahdit (LiftedPrim, LiftedPrimArray, generateLiftedPrimArray, indexLiftedPrimArray, sizeofLiftedPrimArray)
import Data.Bits (Bits (..))

data DspErr = DspErrOddSamples
  deriving stock (Eq, Show)

instance Exception DspErr

type Sel a = LiftedPrimArray a -> Int -> a

selMonoLeft, selMonoRight :: LiftedPrim a => Sel a
selMonoLeft arr i = indexLiftedPrimArray arr (div i 2)
selMonoRight arr i = indexLiftedPrimArray arr (div i 2 + 1)

selMonoAvg :: (LiftedPrim a, Integral a, Bits a) => Sel a
selMonoAvg arr i =
  let !ix = div i 2
      !lval = indexLiftedPrimArray arr ix
      !rval = indexLiftedPrimArray arr (ix + 1)
      !halfLval = div lval 2
      !halfRval = div rval 2
      !extra = lval .&. rval .&. 1
  in halfLval + halfRval + extra

monoFromSel :: LiftedPrim a => (LiftedPrimArray a -> Int -> a) -> LiftedPrimArray a -> Either DspErr (LiftedPrimArray a)
monoFromSel sel src =
  let !srcLen = sizeofLiftedPrimArray src
  in if odd srcLen
    then Left DspErrOddSamples
    else
      let !destLen = div srcLen 2
          !dest = generateLiftedPrimArray destLen (sel src)
      in Right dest

monoFromLeft, monoFromRight :: LiftedPrim a => LiftedPrimArray a -> Either DspErr (LiftedPrimArray a)
monoFromLeft = monoFromSel selMonoLeft
monoFromRight = monoFromSel selMonoRight

monoFromAvg :: (LiftedPrim a, Integral a, Bits a) => LiftedPrimArray a -> Either DspErr (LiftedPrimArray a)
monoFromAvg = monoFromSel selMonoAvg

stereoFromMono :: LiftedPrim a => LiftedPrimArray a -> LiftedPrimArray a
stereoFromMono src =
  let !srcLen = sizeofLiftedPrimArray src
      !destLen = srcLen * 2
  in generateLiftedPrimArray destLen (\i -> indexLiftedPrimArray src (div i 2))
