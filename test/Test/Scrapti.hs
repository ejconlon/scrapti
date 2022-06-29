module Test.Scrapti (testScrapti) where

import Dahdit (Binary (..), ByteCount, ElementCount, Get, Int16LE, PrimArray, Proxy (..), ShortByteString,
               StaticByteSized (..), StaticBytes, Word16LE, Word8, byteSize, getExact, getSkip, getWord32LE, runGetIO,
               runPut)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Default (def)
import Data.Primitive.PrimArray (sizeofPrimArray)
import qualified Data.Sequence as Seq
import Scrapti.Pti (Auto (..), AutoEnvelope (..), AutoType, Block, Effects (..), Filter, FilterType, Granular,
                    GranularLoopMode, GranularShape, Header (..), InstParams (..), Lfo (..), LfoSteps, LfoType,
                    Preamble (..), Pti (..), SamplePlayback, Slices, WavetableWindowSize, calculateCrc)
import Scrapti.Riff (Chunk (..), chunkHeaderSize, getChunkSize, getExpectLabel, labelRiff)
import Scrapti.Sfont (Bag, Gen, InfoChunk (..), Inst, ListChunk (..), Mod, OptChunk (..), PdtaChunk (..), Phdr,
                      Sdta (..), SdtaChunk (..), Sfont (..), Shdr, labelSfbk)
import Scrapti.Wav (Sampled (..), SampledWav (..), Wav (..), WavBody (..), WavChunk (..), WavFormat (..),
                    WavFormatChunk (..), WavHeader (..), WavSampleChunk (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

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
  staticByteSize (Proxy :: Proxy Effects) @?= 4
  staticByteSize (Proxy :: Proxy Header) @?= 388
  -- NOTE: total "header size" is 392 with crc32

testPtiWrite :: TestTree
testPtiWrite = testCase "write" $ do
  bs <- readShort "testdata/testproj/instruments/1 drums.pti"
  (pti, bc) <- runGetIO (get @Pti) bs
  byteSize pti @?= bc
  fromIntegral bc @?= BSS.length bs
  sizeofPrimArray (ptiWav pti) @?= div (fromIntegral drumDataLen) 2
  let bs' = runPut (put pti)
  bs' @?= bs

type Sel a = Header -> a

selPreAux0To19 :: Sel (StaticBytes 20)
selPreAux0To19 = preAux0To19 . hdrPreamble

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
  bs <- readShort "testdata/testproj/instruments/1 drums.pti"
  (actHdr, _) <- runGetIO (get @Header) bs
  let defHdr = def @Header
      same :: (Eq a, Show a) => Int -> Sel a -> IO ()
      same i sel = assertEqual ("aux " ++ show i) (sel defHdr) (sel actHdr)
  same 0 selPreAux0To19
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
  bs <- readShort "testdata/testproj/instruments/1 drums.pti"
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
  let modDefDigest = calculateCrc modDefHdr
  actDigest @?= modDefDigest

testPtiDigest :: TestTree
testPtiDigest = testCase "digest" $ do
  let defPti = def @Pti
      expecDigest = calculateCrc (ptiHeader defPti)
  ptiCrc defPti @?= expecDigest

getWavInt16LE :: Get (Wav Int16LE)
getWavInt16LE = get

extractWavData :: Wav a -> PrimArray a
extractWavData = unWavSampleChunk . wbSample . wavBody

testPtiWav :: TestTree
testPtiWav = testCase "wav" $ do
  wavBs <- readShort "testdata/drums.wav"
  ptiBs <- readShort "testdata/testproj/instruments/1 drums.pti"
  (wav, _) <- runGetIO getWavInt16LE wavBs
  (pti, _) <- runGetIO (get @Pti) ptiBs
  let wavData = extractWavData wav
      ptiData = ptiWav pti
  -- We expect there is half as many samples because it's converted to mono
  sizeofPrimArray ptiData @?= div (sizeofPrimArray wavData) 2
  -- As far as the content, I have no idea what it's doing to the signal...
  -- Can do FFT and compare frequencies?

testPti :: TestTree
testPti = testGroup "pti" [testPtiSizes, testPtiWrite, testPtiAux, testPtiMinimal, testPtiDigest, testPtiWav]

testConvert :: TestTree
testConvert = testCase "convert" $ do
  -- TODO test conversion of sfont
  pure ()

testScrapti :: TestTree
testScrapti = testGroup "Scrapti" [testWav, testSfont, testPti, testConvert]
