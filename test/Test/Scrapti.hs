module Test.Scrapti (testScrapti) where

import Dahdit (Binary (..), ByteCount, ElementCount, Get, Int16LE, Proxy (..), ShortByteString, StaticByteSized (..),
               byteSize, getExact, getSkip, runGetIO, runPut)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Primitive.PrimArray (sizeofPrimArray)
import qualified Data.Sequence as Seq
import Scrapti.Pti (Auto, AutoEnvelope (..), AutoType, Effects (..), Filter, FilterType, Granular, GranularLoopMode,
                    GranularShape, Header (..), InstParams, Lfo, LfoSteps, LfoType, Preamble (..), Pti (..),
                    SamplePlayback, Slices, WavetableWindowSize)
import Scrapti.Riff (Chunk (..), chunkHeaderSize, getChunkSize, getExpectLabel, labelRiff)
import Scrapti.Sfont (Bag, Gen, InfoChunk (..), Inst, ListChunk (..), Mod, OptChunk (..), PdtaChunk (..), Phdr,
                      Sdta (..), SdtaChunk (..), Sfont (..), Shdr, labelSfbk)
import Scrapti.Wav (Sampled (..), SampledWav (..), Wav (..), WavBody (..), WavChunk (..), WavFormat (..),
                    WavFormatChunk (..), WavHeader (..), WavSampleChunk (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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
  (Sfont (InfoChunk (ListChunk infos)) (SdtaChunk (OptChunk maySdta)) (PdtaChunk (ListChunk pdtaBlocks)), _) <- runGetIO (get @Sfont) bs
  Seq.length infos @?= 5
  case maySdta of
    Nothing -> fail "Missing sdta"
    Just sdta -> do
      sizeofPrimArray (sdtaHighBits sdta) @?= 1365026
      sdtaLowBits sdta @?= Nothing
  Seq.length pdtaBlocks @?= 9

testSfontWrite :: TestTree
testSfontWrite = testCase "write" $ do
  bs <- readShort "testdata/timpani.sf2"
  (sfont, _) <- runGetIO (get @Sfont) bs
  let bs' = runPut (put sfont)
  bs' @?= bs

testSfontManual :: TestTree
testSfontManual = testCase "manual" $ do
  bs <- readShort "testdata/timpani.sf2"
  ((info, sdta, pdta), _) <- flip runGetIO bs $ do
    getExpectLabel labelRiff
    chunkSize <- getChunkSize
    getExact chunkSize $ do
      getExpectLabel labelSfbk
      info <- get @InfoChunk
      sdta <- get @SdtaChunk
      pdta <- get @PdtaChunk
      pure (info, sdta, pdta)
  let expecInfoSize = 124 + chunkHeaderSize
      expecSdtaSize = 2730064 + chunkHeaderSize
      expecPdtaSize = 3010 + chunkHeaderSize
  byteSize info @?= expecInfoSize
  byteSize sdta @?= expecSdtaSize
  byteSize pdta @?= expecPdtaSize
  let infoBs = runPut (put info)
  BSS.length infoBs @?= fromIntegral expecInfoSize
  let sdtaBs = runPut (put sdta)
  BSS.length sdtaBs @?= fromIntegral expecSdtaSize
  let pdtaBs = runPut (put pdta)
  BSS.length pdtaBs @?= fromIntegral expecPdtaSize

testSfontSizes :: TestTree
testSfontSizes = testCase "sizes" $ do
  staticByteSize (Proxy :: Proxy Phdr) @?= 38
  staticByteSize (Proxy :: Proxy Bag) @?= 4
  staticByteSize (Proxy :: Proxy Mod) @?= 10
  staticByteSize (Proxy :: Proxy Gen) @?= 4
  staticByteSize (Proxy :: Proxy Inst) @?= 22
  staticByteSize (Proxy :: Proxy Shdr) @?= 46

testSfont :: TestTree
testSfont = testGroup "sfont" [testSfontSizes, testSfontWhole, testSfontWrite, testSfontManual]

testPtiSizes :: TestTree
testPtiSizes = testCase "sizes" $ do
  staticByteSize (Proxy :: Proxy WavetableWindowSize) @?= 2
  staticByteSize (Proxy :: Proxy SamplePlayback) @?= 1
  staticByteSize (Proxy :: Proxy Preamble) @?= 92
  staticByteSize (Proxy :: Proxy AutoEnvelope) @?= 18
  staticByteSize (Proxy :: Proxy AutoType) @?= 2
  staticByteSize (Proxy :: Proxy Auto) @?= 20
  staticByteSize (Proxy :: Proxy LfoType) @?= 1
  staticByteSize (Proxy :: Proxy LfoSteps) @?= 1
  staticByteSize (Proxy :: Proxy Lfo) @?= 8
  staticByteSize (Proxy :: Proxy FilterType) @?= 2
  staticByteSize (Proxy :: Proxy Filter) @?= 10
  staticByteSize (Proxy :: Proxy InstParams) @?= 10
  staticByteSize (Proxy :: Proxy Slices) @?= 98
  staticByteSize (Proxy :: Proxy GranularShape) @?= 1
  staticByteSize (Proxy :: Proxy GranularLoopMode) @?= 1
  staticByteSize (Proxy :: Proxy Granular) @?= 6
  staticByteSize (Proxy :: Proxy Effects) @?= 8
  staticByteSize (Proxy :: Proxy Header) @?= 392

testPtiWrite :: TestTree
testPtiWrite = testCase "write" $ do
  bs <- readShort "testdata/testproj/instruments/1 drums.pti"
  (pti, bc) <- runGetIO (get @Pti) bs
  byteSize pti @?= bc
  fromIntegral bc @?= BSS.length bs
  sizeofPrimArray (ptiWav pti) @?= div (fromIntegral drumDataLen) 2
  let bs' = runPut (put pti)
  bs' @?= bs

testPti :: TestTree
testPti = testGroup "pti" [testPtiSizes, testPtiWrite]

testScrapti :: TestTree
testScrapti = testGroup "Scrapti" [testWav, testSfont, testPti]
