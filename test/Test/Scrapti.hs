module Test.Scrapti (testScrapti) where

import Dahdit (Binary (..), ByteCount, ElementCount, Get, Int16LE, ShortByteString, getSkip, runGetIO, runPut, getExact, Proxy (..), getRemainingSize)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Primitive.PrimArray (sizeofPrimArray)
import qualified Data.Sequence as Seq
import Scrapti.Riff (Chunk (..))
import Scrapti.Sfont (InfoChunk (..), ListChunk (..), PdtaChunk (..), SdtaChunk (..), Sfont (..))
import Scrapti.Wav (Sampled (..), SampledWav (..), Wav (..), WavBody (..), WavChunk (..), WavFormat (..),
                    WavFormatChunk (..), WavHeader (..), WavSampleChunk (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
-- import Debug.Trace (traceM)

dataOffset :: ByteCount
dataOffset = 36

drumFmt :: WavFormatChunk
drumFmt = WavFormatChunk (Chunk (WavFormat 1 2 44100 16 mempty))

drumFileSize :: ByteCount
drumFileSize = 497896

drumEndOffset :: ByteCount
drumEndOffset = drumFileSize + 8

drumHeader :: WavHeader
drumHeader = WavHeader (drumFileSize - 28) drumFmt

drumDataLen :: ElementCount
drumDataLen = 248886

readShort :: FilePath -> IO ShortByteString
readShort = fmap (BSS.toShort . BSL.toStrict) . BSL.readFile

testWavHeader :: TestTree
testWavHeader = testCase "header" $ do
  bs <- readShort "testdata/drums.wav"
  (header, bc) <- runGetIO get bs
  header @?= drumHeader
  bc @?= dataOffset

getChunkInt16LE :: Get (WavChunk Int16LE)
getChunkInt16LE = get

testWavData :: TestTree
testWavData = testCase "data" $ do
  bs <- readShort "testdata/drums.wav"
  (arr, _) <- flip runGetIO bs $ do
    getSkip dataOffset
    chunk <- getChunkInt16LE
    case chunk of
      WavChunkSample (WavSampleChunk arr) -> pure arr
      _ -> fail "expected samples"
  fromIntegral (sizeofPrimArray arr) @?= drumDataLen

-- testWavManual :: TestTree
-- testWavManual = testCase "manual" $ do
--   bs <- readShort "testdata/drums.wav"
--   ((header, body), bc) <- flip runGetIO bs $ do
--     initRem <- getRemainingSize
--     traceM ("XXX INIT REMAINING SIZE: " ++ show initRem)
--     header@(WavHeader remaining fmt) <- get @WavHeader
--     headerRem <- getRemainingSize
--     traceM ("XXX POST HEADER REMAINING SIZE: " ++ show headerRem)
--     traceM ("XXX HEADER FILE REMAINING SIZE: " ++ show remaining)
--     body <- getRestOfWav (Proxy :: Proxy Int16LE) remaining fmt
--     pure (header, body)
--   header @?= drumHeader
--   bc @?= dataOffset

testWavWhole :: TestTree
testWavWhole = testCase "whole" $ do
  bs <- readShort "testdata/drums.wav"
  (SampledWav (Sampled (Wav fmt (WavBody pre _ post))), _) <- runGetIO (get @SampledWav) bs
  fmt @?= drumFmt
  Seq.length pre @?= 0
  Seq.length post @?= 2

testWavWrite :: TestTree
testWavWrite = testCase "write" $ do
  bs <- readShort "testdata/drums.wav"
  (swav, _) <- runGetIO (get @SampledWav) bs
  let bs' = runPut (put swav)
  bs' @?= bs

testWav :: TestTree
testWav = testGroup "wav" [testWavHeader, testWavData, testWavWhole, testWavWrite]

testSfontWhole :: TestTree
testSfontWhole = testCase "whole" $ do
  bs <- readShort "testdata/timpani.sf2"
  (Sfont (InfoChunk (ListChunk infos)) sdta (PdtaChunk (ListChunk pdtaBlocks)), _) <- runGetIO (get @Sfont) bs
  Seq.length infos @?= 5
  sizeofPrimArray (sdtaHighBits sdta) @?= 1365026
  sdtaLowBits sdta @?= Nothing
  Seq.length pdtaBlocks @?= 9

testSfontWrite :: TestTree
testSfontWrite = testCase "write" $ do
  bs <- readShort "testdata/timpani.sf2"
  (sfont, _) <- runGetIO (get @Sfont) bs
  let bs' = runPut (put sfont)
  bs' @?= bs

testSfont :: TestTree
testSfont = testGroup "sfont" [testSfontWhole, testSfontWrite]

testPtiManual :: TestTree
testPtiManual = testCase "manual" $ do
  -- bs <- readshort "testdata/testproj/instruments/1 drums.pti"
  -- pti <- runGetIO (get @PairPti) bs
  -- print ah
  -- print pti
  pure ()

testPti :: TestTree
testPti = testGroup "pti" [testPtiManual]

testScrapti :: TestTree
testScrapti = testGroup "Scrapti" [testWav, testSfont, testPti]
