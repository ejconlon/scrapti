module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int16)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU
import Scrapti.Binary (ByteLength, ByteOffset, DecodeState (..), Get, decodeGet, decodeIO, skip)
import Scrapti.Sample (Sampled (..), sampleGet)
import Scrapti.Wav (Wav (..), WavChunk (..), WavData (..), WavFormat (..), WavHeader (..), decodeAnyWav, decodeWavChunk,
                    decodeWavHeader, encodeAnyWav)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

dataOffset :: Int
dataOffset = 36

drumFmt :: WavFormat
drumFmt = WavFormat 2 44100 16

drumFileSize :: ByteLength
drumFileSize = 497896

drumEndOffset :: ByteOffset
drumEndOffset = drumFileSize + 8

drumHeader :: WavHeader
drumHeader = WavHeader (drumFileSize - 28) drumFmt

drumDataLen :: Int
drumDataLen = 248886

testHeader :: TestTree
testHeader = testCase "header" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  decodeIO bs $ do
    startOff <- gets decStateOffset
    liftIO (startOff @?= 0)
    header <- decodeWavHeader
    liftIO (header @?= drumHeader)
    endOff <- gets decStateOffset
    liftIO (endOff @?= fromIntegral dataOffset)

testData :: TestTree
testData = testCase "data" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  decodeIO bs $ do
    decodeGet (skip dataOffset)
    startOff <- gets decStateOffset
    liftIO (startOff @?= fromIntegral dataOffset)
    chunk <- decodeWavChunk 16 (sampleGet :: Get Int16)
    case chunk of
      WavChunkData (WavData vec) -> liftIO (VU.length vec @?= drumDataLen)
      _ -> fail "expected data"

testWhole :: TestTree
testWhole = testCase "whole" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  decodeIO bs $ do
    Sampled (Wav fmt mid (WavData vec) tra) <- decodeAnyWav
    liftIO (fmt @?= drumFmt)
    liftIO (Seq.length mid @?= 0)
    liftIO (VU.length vec @?= drumDataLen)
    liftIO (Seq.length tra @?= 2)
    endInp <- gets decStateInput
    liftIO (assertBool "expected file end" (BSL.null endInp))
    endOff <- gets decStateOffset
    liftIO (endOff @?= drumEndOffset)

testWrite :: TestTree
testWrite = testCase "write" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  swav <- decodeIO bs decodeAnyWav
  let bs' = encodeAnyWav swav
  bs' @?= bs

main :: IO ()
main = defaultMain (testGroup "Scrapti" [testHeader, testData, testWhole, testWrite])
