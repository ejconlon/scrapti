module Main (main) where

import Control.Monad.State.Strict (gets)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence as Seq
import qualified Data.Vector.Primitive as VP
import Scrapti.Binary (BinaryParser (..), ByteLength, ByteOffset, DecodeState (..), Int16LE, ParseM, WithByteSize (..),
                       decodeFail, decodeGet, get, parseSkip, parseWithSize, put, runParseM, runPut, skip)
import Scrapti.Riff (Chunk (..))
import Scrapti.Sfont (InfoChunk (..), PdtaChunk (..), Sdta (..), Sfont (..), decodeSfont, encodeSfont)
import Scrapti.Wav (Sampled (..), SampledWav (..), Wav (..), WavBody (..), WavChunk (..), WavFormatChunk (..),
                    WavFormatData (..), WavHeader (..), WavSampleChunk (..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

dataOffset :: ByteLength
dataOffset = 36

drumFmt :: WavFormatChunk
drumFmt = WavFormatChunk (Chunk (WavFormatData 1 2 44100 16 mempty))

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

-- testSfontWhole :: TestTree
-- testSfontWhole = testCase "whole" $ do
--   bs <- BSL.readFile "testdata/timpani.sf2"
--   Sfont (InfoChunk infos) sdta (PdtaChunk pdtaBlocks) <- decodeFail bs decodeSfont
--   Seq.length infos @?= 5
--   VP.length (unWavData (sdtaHighBits sdta)) @?= 1365026
--   sdtaLowBits sdta @?= Nothing
--   Seq.length pdtaBlocks @?= 9

-- testSfontWrite :: TestTree
-- testSfontWrite = testCase "write" $ do
--   bs <- BSL.readFile "testdata/timpani.sf2"
--   sfont <- decodeFail bs decodeSfont
--   let bs' = encodeSfont sfont
--   bs' @?= bs

-- testSfont :: TestTree
-- testSfont = testGroup "sfont" [testSfontWhole, testSfontWrite]

main :: IO ()
main = defaultMain (testGroup "Scrapti" [testWav]) --, testSfont])
