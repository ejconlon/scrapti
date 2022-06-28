module Scrapti.Dsp
  ( DspError (..)
  , monoFromLeft
  , monoFromRight
  , monoFromAvg
  , stereoFromMono
  ) where

import Control.Exception (Exception)
import Data.Bits (Bits (..))
import Data.Primitive (Prim)
import Data.Primitive.PrimArray (PrimArray, generatePrimArray, indexPrimArray, sizeofPrimArray)

data DspError = DspErrorOddSamples
  deriving stock (Eq, Show)

instance Exception DspError

monoFromSel :: Prim a => (PrimArray a -> Int -> a) -> PrimArray a -> Either DspError (PrimArray a)
monoFromSel sel src =
  let !srcLen = sizeofPrimArray src
  in if odd srcLen
    then Left DspErrorOddSamples
    else
      let !destLen = div srcLen 2
          !dest = generatePrimArray destLen (sel src)
      in Right dest

monoFromLeft, monoFromRight :: Prim a => PrimArray a -> Either DspError (PrimArray a)
monoFromLeft = monoFromSel (\arr i -> indexPrimArray arr (div i 2))
monoFromRight = monoFromSel (\arr i -> indexPrimArray arr (div i 2 + 1))

monoFromAvg :: (Prim a, Integral a, Bits a) => PrimArray a -> Either DspError (PrimArray a)
monoFromAvg = monoFromSel $ \arr i ->
  let !ix = div i 2
      !lval = indexPrimArray arr ix
      !rval = indexPrimArray arr (ix + 1)
      !halfLval = div lval 2
      !halfRval = div rval 2
      !extra = lval .&. rval .&. 1
  in halfLval + halfRval + extra

stereoFromMono :: Prim a => PrimArray a -> PrimArray a
stereoFromMono src =
  let !srcLen = sizeofPrimArray src
      !destLen = srcLen * 2
  in generatePrimArray destLen (\i -> indexPrimArray src (div i 2))
