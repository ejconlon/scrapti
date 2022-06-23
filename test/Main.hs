module Main (main) where

import Control.Monad.State.Strict (gets)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence as Seq
import qualified Data.Vector.Primitive as VP
import Scrapti.Binary (ByteLength, ByteOffset, DecodeState (..), Int16LE, decodeFail, decodeGet, get, skip)
import Scrapti.Sfont (InfoChunk (..), PdtaChunk (..), Sdta (..), Sfont (..), decodeSfont, encodeSfont)
import Scrapti.Wav (Sampled (..), Wav (..), WavChunk (..), WavData (..), WavFormat (..), WavHeader (..), decodeAnyWav,
                    decodeWavChunk, decodeWavHeader, encodeAnyWav)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

dataOffset :: Int
dataOffset = 36

drumFmt :: WavFormat
drumFmt = WavFormat 2 44100 16

drumFileSize :: ByteLength
drumFileSize = 497896

drumEndOffset :: ByteOffset
drumEndOffset = fromIntegral drumFileSize + 8

drumHeader :: WavHeader
drumHeader = WavHeader (drumFileSize - 28) drumFmt

drumDataLen :: Int
drumDataLen = 248886

testWavHeader :: TestTree
testWavHeader = testCase "header" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  (startOff, header, endOff) <- decodeFail bs $ do
    startOff <- gets decStateOffset
    header <- decodeWavHeader
    endOff <- gets decStateOffset
    pure (startOff, header, endOff)
  startOff @?= 0
  header @?= drumHeader
  endOff @?= fromIntegral dataOffset

testWavData :: TestTree
testWavData = testCase "data" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  (startOff, vec) <- decodeFail bs $ do
    decodeGet (skip dataOffset)
    startOff <- gets decStateOffset
    chunk <- decodeWavChunk 16 (get @Int16LE)
    case chunk of
      WavChunkData (WavData vec) -> pure (startOff, vec)
      _ -> fail "expected data"
  startOff @?= fromIntegral dataOffset
  VP.length vec @?= drumDataLen

testWavWhole :: TestTree
testWavWhole = testCase "whole" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  (swav, endInp, endOff) <- decodeFail bs $ do
    swav <- decodeAnyWav
    endInp <- gets decStateInput
    endOff <- gets decStateOffset
    pure (swav, endInp, endOff)
  let Sampled (Wav fmt mid _ tra) = swav
  fmt @?= drumFmt
  Seq.length mid @?= 0
  Seq.length tra @?= 2
  assertBool "expected file end" (BSL.null endInp)
  endOff @?= drumEndOffset

testWavWrite :: TestTree
testWavWrite = testCase "write" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  swav <- decodeFail bs decodeAnyWav
  let bs' = encodeAnyWav swav
  bs' @?= bs

testWav :: TestTree
testWav = testGroup "wav" [testWavHeader, testWavData, testWavWhole, testWavWrite]

testSfontWhole :: TestTree
testSfontWhole = testCase "whole" $ do
  bs <- BSL.readFile "testdata/timpani.sf2"
  Sfont (InfoChunk infos) sdta (PdtaChunk pdtaBlocks) <- decodeFail bs decodeSfont
  Seq.length infos @?= 5
  VP.length (unWavData (sdtaHighBits sdta)) @?= 1365026
  sdtaLowBits sdta @?= Nothing
  Seq.length pdtaBlocks @?= 9

testSfontWrite :: TestTree
testSfontWrite = testCase "write" $ do
  bs <- BSL.readFile "testdata/timpani.sf2"
  sfont <- decodeFail bs decodeSfont
  let bs' = encodeSfont sfont
  bs' @?= bs

testSfont :: TestTree
testSfont = testGroup "sfont" [testSfontWhole, testSfontWrite]

main :: IO ()
main = defaultMain (testGroup "Scrapti" [testWav, testSfont])
