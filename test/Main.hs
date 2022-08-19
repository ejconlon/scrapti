{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (Exception, throwIO)
import Dahdit (Binary (..), ByteCount, ElementCount, Get, ShortByteString, StaticByteSized (..), StaticBytes, Word16LE,
               Word8, byteSize, getExact, getSkip, getWord32LE, runGetIO, runPut, sizeofLiftedPrimArray)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Default (def)
import Data.Foldable (for_)
import Data.Primitive.ByteArray (sizeofByteArray)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Scrapti.Aiff (Aiff (..))
import Scrapti.Binary (QuietArray (..), QuietLiftedArray (..))
import Scrapti.Common (UnparsedBody (..), chunkHeaderSize, defaultLoopMarkNames, getChunkSizeLE, getExpectLabel,
                       guardChunk)
import Scrapti.Convert (Neutral (..), aiffToNeutral, neutralToWav)
import Scrapti.Riff (Chunk (..), KnownChunk (..), KnownListChunk (..), KnownOptChunk (..), labelRiff)
import Scrapti.Sfont (Bag, Gen, InfoChunk (..), Inst, Mod, PdtaChunk (..), Phdr, Sdta (..), SdtaChunk (..), Sfont (..),
                      Shdr, labelSfbk)
import Scrapti.Tracker.Checked (Checked (..), mkCode)
import Scrapti.Tracker.Loader (Overwrite (..), loadRichProject, saveRichProject)
import Scrapti.Tracker.Mt (Mt)
import Scrapti.Tracker.Mtp (Mtp)
import Scrapti.Tracker.Pti (Auto (..), AutoEnvelope (..), AutoType, Block, Effects (..), Filter, FilterType, Granular,
                            GranularLoopMode, GranularShape, Header (..), InstParams (..), Lfo (..), LfoSteps, LfoType,
                            Preamble (..), Pti (..), SamplePlayback, Slices, WavetableWindowSize)
import Scrapti.Wav (Wav (..), WavAdtlChunk, WavAdtlData (..), WavAdtlElem (..), WavChunk (..), WavDataBody (..),
                    WavFormatBody (..), WavFormatChunk, WavHeader (..), WavInfoElem (..), lookupWavDataChunk,
                    lookupWavFormatChunk, wavToPcmContainer)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

rethrow :: Exception e => Either e a -> IO a
rethrow = either throwIO pure

drumFmtOffset :: ByteCount
drumFmtOffset = 12

drumDataOffset :: ByteCount
drumDataOffset = 36

drumPostDataOffset :: ByteCount
drumPostDataOffset = 497816

drumFmtChunk :: WavFormatChunk
drumFmtChunk = KnownChunk (WavFormatBody 1 2 44100 16 mempty)

drumFileSize :: ByteCount
drumFileSize = 497896

drumEndOffset :: ByteCount
drumEndOffset = drumFileSize + 8

drumHeader :: WavHeader
drumHeader = WavHeader (drumFileSize - 4)

drumDataLen :: ElementCount
drumDataLen = 248886

readShort :: FilePath -> IO ShortByteString
readShort = fmap (BSS.toShort . BSL.toStrict) . BSL.readFile

testWavSerde :: TestTree
testWavSerde = testCase "serde" $ do
  -- KnownChunk
  byteSize drumFmtChunk @?= 24
  let fmtBs = runPut (put drumFmtChunk)
  (fmt, fmtSz) <- runGetIO get fmtBs
  byteSize fmt @?= fmtSz
  fmt @?= drumFmtChunk
  -- Odd and even unparsed chunks
  let unpSz = 10
      unpOdd = Chunk "FOOB" (UnparsedBody "A")
      unpEven = Chunk "FOOB" (UnparsedBody "AB")
  byteSize unpOdd @?= unpSz
  byteSize unpEven @?= unpSz
  let unpOddBs = runPut (put unpOdd)
  (unpOdd', unpOddSz) <- runGetIO get unpOddBs
  byteSize unpOdd' @?= unpOddSz
  unpOdd' @?= unpOdd
  let unpEvenBs = runPut (put unpEven)
  (unpEven', unpEvenSz) <- runGetIO get unpEvenBs
  byteSize unpEven' @?= unpEvenSz
  unpEven' @?= unpEven
  -- Adtl chunks
  let adtlChunkEven = KnownListChunk (Seq.singleton (WavAdtlElem 42 (WavAdtlDataNote "hi")))
  assertReparses @WavAdtlChunk adtlChunkEven
  let adtlChunkOdd = KnownListChunk (Seq.singleton (WavAdtlElem 42 (WavAdtlDataNote "h")))
  assertReparses @WavAdtlChunk adtlChunkOdd

testWavHeader :: TestTree
testWavHeader = testCase "header" $ do
  bs <- readShort "testdata/drums.wav"
  (header, bc) <- runGetIO get bs
  header @?= drumHeader
  bc @?= drumFmtOffset

testWavData :: TestTree
testWavData = testCase "data" $ do
  bs <- readShort "testdata/drums.wav"
  (arr, _) <- flip runGetIO bs $ do
    getSkip drumDataOffset
    chunk <- get
    case chunk of
      WavChunkData (KnownChunk (WavDataBody (QuietArray arr))) -> pure arr
      _ -> fail "expected data"
  fromIntegral (sizeofByteArray arr) @?= drumDataLen * 2  -- x2 for 2-byte samples

testWavInfo :: TestTree
testWavInfo = testCase "info" $ do
  bs <- readShort "testdata/drums.wav"
  (info, _) <- flip runGetIO bs $ do
    getSkip drumPostDataOffset
    chunk <- get
    case chunk of
      WavChunkInfo (KnownListChunk info) -> pure info
      _ -> fail "expected info"
  info @?= Seq.fromList [WavInfoElem "IART" "freewavesamples.com"]

testWavWhole :: TestTree
testWavWhole = testCase "whole" $ do
  bs <- readShort "testdata/drums.wav"
  (wav, _) <- runGetIO (get @Wav) bs
  fmtChunk <- rethrow (guardChunk "format" (lookupWavFormatChunk wav))
  fmtChunk @?= drumFmtChunk
  KnownChunk (WavDataBody (QuietArray arr)) <- rethrow (guardChunk "data" (lookupWavDataChunk wav))
  fromIntegral (sizeofByteArray arr) @?= drumDataLen * 2 -- x2 for 2-byte samples
  Seq.length (wavChunks wav) @?= 4

testWavWrite :: TestTree
testWavWrite = testCase "write" $ do
  bs <- readShort "testdata/drums.wav"
  (wav, bc) <- runGetIO (get @Wav) bs
  byteSize wav @?= bc
  for_ (wavChunks wav) assertReparses
  let bs' = runPut (put wav)
  bs' @?= bs

testWavWrite2 :: TestTree
testWavWrite2 = testCase "write" $ do
  bs <- readShort "testdata/DX-EPiano1-C1.wav"
  (wav, bc) <- runGetIO (get @Wav) bs
  byteSize wav @?= bc
  for_ (wavChunks wav) assertReparses
  let bs' = runPut (put wav)
  bs' @?= bs

testWav :: TestTree
testWav = testGroup "wav" [testWavSerde, testWavHeader, testWavData, testWavInfo, testWavWhole, testWavWrite, testWavWrite2]

testAiff :: TestTree
testAiff = testCase "aiff" $ do
  bs <- readShort "testdata/M1F1-int16s-AFsp.aif"
  (aiff, bc) <- runGetIO (get :: Get Aiff) bs
  byteSize aiff @?= bc
  bc @?= fromIntegral (BSS.length bs)
  for_ (aiffChunks aiff) assertReparses
  let bs' = runPut (put aiff)
  fromIntegral (BSS.length bs') @?= bc
  bs' @?= bs

testSfontWhole :: TestTree
testSfontWhole = testCase "whole" $ do
  bs <- readShort "testdata/timpani.sf2"
  (Sfont (InfoChunk (KnownListChunk infos)) (SdtaChunk (KnownOptChunk maySdta)) (PdtaChunk (KnownListChunk pdtaBlocks)), _) <- runGetIO (get @Sfont) bs
  Seq.length infos @?= 5
  case maySdta of
    Nothing -> fail "Missing sdta"
    Just sdta -> do
      sizeofLiftedPrimArray (sdtaHighBits sdta) @?= 1365026
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
    chunkSize <- getChunkSizeLE
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
  staticByteSize (Proxy :: Proxy Effects) @?= 4
  staticByteSize (Proxy :: Proxy Header) @?= 388
  -- NOTE: total "header size" is 392 with crc32

testPtiWrite :: TestTree
testPtiWrite = testCase "write" $ do
  bs <- readShort "testdata/testproj16/instruments/1 drums.pti"
  (pti, bc) <- runGetIO (get @Pti) bs
  byteSize pti @?= bc
  fromIntegral bc @?= BSS.length bs
  -- divide by two to compare number of elements (2-byte samples)
  sizeofLiftedPrimArray (unQuietLiftedArray (ptiPcmData pti)) @?= div (fromIntegral drumDataLen) 2
  let bs' = runPut (put pti)
  bs' @?= bs

type Sel a = Header -> a

selPreAux2To19 :: Sel (StaticBytes 18)
selPreAux2To19 = preAux2To19 . hdrPreamble

selPreAux52To59 :: Sel (StaticBytes 8)
selPreAux52To59 = preAux52To59 . hdrPreamble

selPreAux66To67 :: Sel Word16LE
selPreAux66To67 = preAux66To67 . hdrPreamble

selPreAux70To75 :: Sel (StaticBytes 6)
selPreAux70To75 = preAux70To75 . hdrPreamble

selPreAux77 :: Sel Word8
selPreAux77 = preAux77 . hdrPreamble

selPreAux86To87 :: Sel Word16LE
selPreAux86To87 = preAux86To87 . hdrPreamble

selPreAux90To91 :: Sel Word16LE
selPreAux90To91 = preAux90To91 . hdrPreamble

selAeAux96To97, selAeAux100To101 :: Sel (Block Word16LE)
selAeAux96To97 = fmap (aeAux96To97 . autoEnvelope) . hdrAutoBlock
selAeAux100To101 = fmap (aeAux100To101 . autoEnvelope) . hdrAutoBlock

selLfoAux214To215 :: Sel (Block Word16LE)
selLfoAux214To215 = fmap lfoAux214To215 . hdrLfoBlock

selIpAux273To275 :: Sel (StaticBytes 3)
selIpAux273To275 = ipAux273To275 . hdrInstParams

selIpAux277, selIpAux279 :: Sel Word8
selIpAux277 = ipAux277 . hdrInstParams
selIpAux279 = ipAux279 . hdrInstParams

selEffAux387 :: Sel Word8
selEffAux387 = effAux387 . hdrEffects

testPtiAux :: TestTree
testPtiAux = testCase "aux" $ do
  bs <- readShort "testdata/testproj16/instruments/1 drums.pti"
  (actHdr, _) <- runGetIO (get @Header) bs
  let defHdr = def @Header
      same :: (Eq a, Show a) => Int -> Sel a -> IO ()
      same i sel = assertEqual ("aux " ++ show i) (sel defHdr) (sel actHdr)
  same 2 selPreAux2To19
  same 52 selPreAux52To59
  same 66 selPreAux66To67
  same 70 selPreAux70To75
  same 77 selPreAux77
  same 86 selPreAux86To87
  same 90 selPreAux90To91
  same 96 selAeAux96To97
  same 100 selAeAux100To101
  same 214 selLfoAux214To215
  same 273 selIpAux273To275
  same 277 selIpAux277
  same 279 selIpAux279
  same 387 selEffAux387

testPtiMinimal :: TestTree
testPtiMinimal = testCase "minimal" $ do
  bs <- readShort "testdata/testproj16/instruments/1 drums.pti"
  ((actHdr, actDigest), _) <- runGetIO ((,) <$> get @Header <*> getWord32LE) bs
  let defHdr = def @Header
      actPre = hdrPreamble actHdr
      defPre = hdrPreamble defHdr
      modDefPre = defPre { preName = preName actPre }
      modDefHdr = defHdr { hdrPreamble = modDefPre }
  -- Check individually
  -- For preamble, need to set name
  actPre @?= modDefPre
  hdrAutoBlock actHdr @?= hdrAutoBlock defHdr
  hdrLfoBlock actHdr @?= hdrLfoBlock defHdr
  hdrFilter actHdr @?= hdrFilter defHdr
  hdrInstParams actHdr @?= hdrInstParams defHdr
  hdrSlices actHdr @?= hdrSlices defHdr
  hdrGranular actHdr @?= hdrGranular defHdr
  hdrEffects actHdr @?= hdrEffects defHdr
  -- Now check altogether
  actHdr @?= modDefHdr
  -- Now check digest
  let modDefDigest = mkCode modDefHdr
  actDigest @?= modDefDigest

testPtiDigest :: TestTree
testPtiDigest = testCase "digest" $ do
  let defPti = def @Pti
      expecDigest = mkCode (checkedVal (ptiHeader defPti))
  checkedCode (ptiHeader defPti) @?= expecDigest

testPtiWav :: TestTree
testPtiWav = testCase "wav" $ do
  wavBs <- readShort "testdata/drums.wav"
  ptiBs <- readShort "testdata/testproj16/instruments/1 drums.pti"
  (wav, _) <- runGetIO (get @Wav) wavBs
  (pti, _) <- runGetIO (get @Pti) ptiBs
  KnownChunk (WavDataBody (QuietArray wavData)) <- rethrow (guardChunk "data" (lookupWavDataChunk wav))
  -- We expect there are half as many samples because it's converted to mono
  -- divide by four to compare number of elements (2-byte samples and 2 channels)
  sizeofLiftedPrimArray (unQuietLiftedArray (ptiPcmData pti)) @?= div (sizeofByteArray wavData) 4
  -- As far as the content, I have no idea what it's doing to the signal...

testPti :: TestTree
testPti = testGroup "pti" [testPtiSizes, testPtiWrite, testPtiAux, testPtiMinimal, testPtiDigest, testPtiWav]

testOtherSizes :: TestTree
testOtherSizes = testCase "other sizes" $ do
  staticByteSize (Proxy :: Proxy Mt) @?= 1796
  staticByteSize (Proxy :: Proxy Mtp) @?= 6184

testProject :: TestTree
testProject = testCase "project" $ do
  p <- loadRichProject "testdata/testproj16"
  withSystemTempDirectory "testproj16" $ \path -> do
    saveRichProject OverwriteYesReally path p
    songExists <- doesFileExist (path </> "project.mt")
    assertBool "song does not exist" songExists
    q <- loadRichProject path
    q @?= p

assertReparses :: (Binary a, Eq a, Show a) => a -> IO ()
assertReparses a = do
  let !bs = runPut (put a)
  (a', bc) <- runGetIO get bs
  byteSize a' @?= bc
  a' @?= a

testConvert :: TestTree
testConvert = testCase "convert" $ do
  bs <- readShort "testdata/DX-EPiano1-C1.aif"
  (aif, _) <- runGetIO get bs
  ne <- rethrow (aiffToNeutral 44100 aif (Just defaultLoopMarkNames))
  -- Test that a standard translation of the wav works
  let !wav = neutralToWav ne
  pc <- rethrow (wavToPcmContainer wav)
  pc @?= neCon ne
  assertReparses wav

testScrapti :: TestTree
testScrapti = testGroup "Scrapti" [testWav, testAiff, testSfont, testPti, testProject, testConvert, testOtherSizes]

main :: IO ()
main = defaultMain testScrapti
