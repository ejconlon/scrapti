module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int16)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Primitive as VP
import Scrapti.Binary (ByteLength, ByteOffset, DecodeState (..), Get, Int16LE, decodeFail, decodeGet, get, skip)
import Scrapti.Sample (Sampled (..))
import Scrapti.Sfont (Sdta (..), Sfont (..), decodeSfont, encodeSfont)
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

testWavHeader :: TestTree
testWavHeader = testCase "header" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  decodeFail bs $ do
    startOff <- gets decStateOffset
    liftIO (startOff @?= 0)
    header <- decodeWavHeader
    liftIO (header @?= drumHeader)
    endOff <- gets decStateOffset
    liftIO (endOff @?= fromIntegral dataOffset)

testWavData :: TestTree
testWavData = testCase "data" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  decodeFail bs $ do
    decodeGet (skip dataOffset)
    startOff <- gets decStateOffset
    liftIO (startOff @?= fromIntegral dataOffset)
    chunk <- decodeWavChunk 16 (get @Int16LE)
    case chunk of
      WavChunkData (WavData vec) -> liftIO (VP.length vec @?= drumDataLen)
      _ -> fail "expected data"

testWavWhole :: TestTree
testWavWhole = testCase "whole" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  decodeFail bs $ do
    Sampled (Wav fmt mid (WavData vec) tra) <- decodeAnyWav
    liftIO (fmt @?= drumFmt)
    liftIO (Seq.length mid @?= 0)
    liftIO (VP.length vec @?= drumDataLen)
    liftIO (Seq.length tra @?= 2)
    endInp <- gets decStateInput
    liftIO (assertBool "expected file end" (BSL.null endInp))
    endOff <- gets decStateOffset
    liftIO (endOff @?= drumEndOffset)

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
  Sfont infos sdta pdtaBlocks <- decodeFail bs decodeSfont
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
