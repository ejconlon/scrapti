module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence as Seq
import qualified Data.Vector.Primitive as VP
import Scrapti.Binary (BinaryParser (..), ByteLength, Int16LE, ParseM, WithByteSize (..), parseSkip, parseWithSize, put,
                       runParseM, runPut)
import Scrapti.Classes (Pair (..))
import Scrapti.Pti (PairPti (..))
import Scrapti.Riff (Chunk (..))
import Scrapti.Sfont (InfoChunk (..), ListChunk (..), PdtaChunk (..), SdtaChunk (..), Sfont (..))
import Scrapti.Wav (Sampled (..), SampledWav (..), Wav (..), WavBody (..), WavChunk (..), WavFormat (..),
                    WavFormatChunk (..), WavHeader (..), WavSampleChunk (..))
import Test.Dahdit (testDahdit)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

dataOffset :: ByteLength
dataOffset = 36

drumFmt :: WavFormatChunk
drumFmt = WavFormatChunk (Chunk (WavFormat 1 2 44100 16 mempty))

drumFileSize :: ByteLength
drumFileSize = 497896

drumEndOffset :: ByteLength
drumEndOffset = drumFileSize + 8

drumHeader :: WavHeader
drumHeader = WavHeader (drumFileSize - 28) drumFmt

drumDataLen :: Int
drumDataLen = 248886

testWavHeader :: TestTree
testWavHeader = testCase "header" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  WithByteSize size header <- runParseM bs parseWithSize
  header @?= drumHeader
  size @?= dataOffset

parseChunkInt16LE :: ParseM (WavChunk Int16LE)
parseChunkInt16LE = parseWithoutSize

testWavData :: TestTree
testWavData = testCase "data" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  vec <- runParseM bs $ do
    parseSkip dataOffset
    chunk <- parseChunkInt16LE
    case chunk of
      WavChunkSample (WavSampleChunk vec) -> pure vec
      _ -> fail "expected samples"
  VP.length vec @?= drumDataLen

testWavWhole :: TestTree
testWavWhole = testCase "whole" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  SampledWav (Sampled (Wav fmt (WavBody pre _ post))) <- runParseM bs (parseWithoutSize @SampledWav)
  fmt @?= drumFmt
  Seq.length pre @?= 0
  Seq.length post @?= 2

testWavWrite :: TestTree
testWavWrite = testCase "write" $ do
  bs <- BSL.readFile "testdata/drums.wav"
  swav <- runParseM bs (parseWithoutSize @SampledWav)
  let bs' = runPut (put swav)
  bs' @?= bs

testWav :: TestTree
testWav = testGroup "wav" [testWavHeader, testWavData, testWavWhole, testWavWrite]

testSfontWhole :: TestTree
testSfontWhole = testCase "whole" $ do
  bs <- BSL.readFile "testdata/timpani.sf2"
  Sfont (InfoChunk (ListChunk infos)) sdta (PdtaChunk (ListChunk pdtaBlocks)) <- runParseM bs (parseWithoutSize @Sfont)
  Seq.length infos @?= 5
  VP.length (sdtaHighBits sdta) @?= 1365026
  sdtaLowBits sdta @?= Nothing
  Seq.length pdtaBlocks @?= 9

testSfontWrite :: TestTree
testSfontWrite = testCase "write" $ do
  bs <- BSL.readFile "testdata/timpani.sf2"
  sfont <- runParseM bs (parseWithoutSize @Sfont)
  let bs' = runPut (put sfont)
  bs' @?= bs

testSfont :: TestTree
testSfont = testGroup "sfont" [testSfontWhole, testSfontWrite]

testPtiManual :: TestTree
testPtiManual = testCase "manual" $ do
  -- bs <- BSL.readFile "testdata/testproj/instruments/1 drums.pti"
  -- PairPti (Pair ah pti) <- runParseM bs (parseWithoutSize @PairPti)
  -- print ah
  -- print pti
  pure ()

testPti :: TestTree
testPti = testGroup "pti" [testPtiManual]

main :: IO ()
-- main = defaultMain (testGroup "Scrapti" [testWav, testSfont, testPti, testDahdit])
main = defaultMain (testGroup "Scrapti" [testDahdit])
