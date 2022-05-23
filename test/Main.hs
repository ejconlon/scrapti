module Main (main) where

import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as BSL
import Scrapti.Binary (DecodeSuccess (..))
import Scrapti.Wav (WavFormat (..), WavHeader (..), decodeWavHeader)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testHeader :: TestTree
testHeader = testCase "header" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  let res = decodeWavHeader bs
  DecodeSuccess off header <- either throwIO pure res
  off @?= 36
  wavHeaderFileSize header @?= 497896
  let fmt = wavHeaderFormat header
  wfNumChannels fmt @?= 2
  wfSampleRate fmt @?= 44100
  wfBitsPerSample fmt @?= 16

main :: IO ()
main = defaultMain (testGroup "Scrapti" [testHeader])
