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
import Data.Bits (Bits (..))
import Data.Primitive (Prim)
import Data.Primitive.PrimArray (PrimArray, generatePrimArray, indexPrimArray, sizeofPrimArray)

data DspErr = DspErrOddSamples
  deriving stock (Eq, Show)

instance Exception DspErr

type Sel a = PrimArray a -> Int -> a

selMonoLeft, selMonoRight :: Prim a => Sel a
selMonoLeft arr i = indexPrimArray arr (div i 2)
selMonoRight arr i = indexPrimArray arr (div i 2 + 1)

selMonoAvg :: (Prim a, Integral a, Bits a) => Sel a
selMonoAvg arr i =
  let !ix = div i 2
      !lval = indexPrimArray arr ix
      !rval = indexPrimArray arr (ix + 1)
      !halfLval = div lval 2
      !halfRval = div rval 2
      !extra = lval .&. rval .&. 1
  in halfLval + halfRval + extra

monoFromSel :: Prim a => (PrimArray a -> Int -> a) -> PrimArray a -> Either DspErr (PrimArray a)
monoFromSel sel src =
  let !srcLen = sizeofPrimArray src
  in if odd srcLen
    then Left DspErrOddSamples
    else
      let !destLen = div srcLen 2
          !dest = generatePrimArray destLen (sel src)
      in Right dest

monoFromLeft, monoFromRight :: Prim a => PrimArray a -> Either DspErr (PrimArray a)
monoFromLeft = monoFromSel selMonoLeft
monoFromRight = monoFromSel selMonoRight

monoFromAvg :: (Prim a, Integral a, Bits a) => PrimArray a -> Either DspErr (PrimArray a)
monoFromAvg = monoFromSel selMonoAvg

stereoFromMono :: Prim a => PrimArray a -> PrimArray a
stereoFromMono src =
  let !srcLen = sizeofPrimArray src
      !destLen = srcLen * 2
  in generatePrimArray destLen (\i -> indexPrimArray src (div i 2))
