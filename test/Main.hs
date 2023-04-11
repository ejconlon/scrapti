{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (Exception, throwIO)
import Control.Monad ((>=>))
import Dahdit
  ( Binary (..)
  , BinaryTarget (..)
  , ByteCount
  , ByteSized
  , ElemCount (..)
  , Get
  , Int16LE (..)
  , ShortByteString
  , StaticByteSized (..)
  , StaticBytes
  , Word16LE
  , Word8
  , byteSize
  , decode
  , decodeFile
  , encode
  , encodeFile
  , getExact
  , getSkip
  , getWord32LE
  , lengthLiftedPrimArray
  , liftedPrimArrayFromList
  )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Foldable (for_, toList)
import Data.Int (Int8)
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray (indexByteArray, sizeofByteArray)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as TIO
import Scrapti.Aiff (Aiff (..), AiffDataBody (..), lookupAiffDataChunk)
import qualified Scrapti.Aiff as Aiff
import Scrapti.Binary (QuietArray (..), QuietLiftedArray (..))
import Scrapti.Common
  ( LoopMarks (..)
  , UnparsedBody (..)
  , chunkHeaderSize
  , defaultLoopMarkNames
  , defaultNoteNumber
  , defineLoopMarks
  , getChunkSizeLE
  , getExpectLabel
  , guardChunk
  , rethrow
  )
import Scrapti.Convert
  ( Neutral (..)
  , aiffToNeutral
  , loadNeutral
  , neutralMono
  , neutralToSampleWav
  , neutralToWav
  , wavToNeutral
  )
import Scrapti.Dsp (ModMeta (..), PcmContainer (..), linearCrossFade, monoFromLeft, runMod)
import Scrapti.Midi.Notes (NoteName (..), OctNote (..), Octave (..))
import Scrapti.Patches.ConvertPti (PtiPatch (..), instToPtiPatches)
import Scrapti.Patches.ConvertSfz (instToSfz, sfzToInst)
import Scrapti.Patches.Inst (InstDef (..), InstKeyRange (..), InstSpec (..), jsonToInst)
import Scrapti.Patches.Loader (Sample (..), defaultInst, matchSamples)
import Scrapti.Patches.Sfz (parseSfz, renderSfz, sfzFileSimilar)
import Scrapti.Riff (Chunk (..), KnownChunk (..), KnownListChunk (..), KnownOptChunk (..), labelRiff)
import Scrapti.Sfont
  ( Bag
  , Gen
  , InfoChunk (..)
  , Inst
  , Mod
  , PdtaChunk (..)
  , Phdr
  , Sdta (..)
  , SdtaChunk (..)
  , Sfont (..)
  , Shdr
  , labelSfbk
  )
import Scrapti.Tracker.Checked (Checked (..), mkCode)
import Scrapti.Tracker.Loader (Overwrite (..), loadRichProject, saveRichProject)
import Scrapti.Tracker.Mt (Mt)
import Scrapti.Tracker.Mtp (Mtp)
import Scrapti.Tracker.Pti
  ( Auto (..)
  , AutoEnvelope (..)
  , AutoType
  , Block
  , Effects (..)
  , Filter
  , FilterType
  , Granular
  , GranularLoopMode
  , GranularShape
  , Header (..)
  , InstParams (..)
  , Lfo (..)
  , LfoSteps
  , LfoType
  , Preamble (..)
  , Pti (..)
  , SamplePlayback
  , Slices
  , WavetableWindowSize
  )
import Scrapti.Wav
  ( Wav (..)
  , WavAdtlChunk
  , WavAdtlData (..)
  , WavAdtlElem (..)
  , WavChunk (..)
  , WavDataBody (..)
  , WavFormatBody (..)
  , WavFormatChunk
  , WavHeader (..)
  , WavInfoElem (..)
  , lookupWavDataChunk
  , lookupWavFormatChunk
  , wavToPcmContainer
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

-- import Text.Pretty.Simple (pPrint)

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

drumDataLen :: ElemCount
drumDataLen = 248886

readShort :: FilePath -> IO ShortByteString
readShort = fmap (BSS.toShort . BSL.toStrict) . BSL.readFile

throwFirst :: Exception e => (Either e a, b) -> IO (a, b)
throwFirst (ea, b) =
  case ea of
    Left e -> throwIO e
    Right a -> pure (a, b)

decodeThrow :: Binary a => ShortByteString -> IO (a, ByteCount)
decodeThrow = throwFirst . decode

getTargetThrow :: Get a -> ShortByteString -> IO (a, ByteCount)
getTargetThrow g z = throwFirst (getTarget g z)

decodeFileThrow :: Binary a => FilePath -> IO (a, ByteCount)
decodeFileThrow = decodeFile >=> throwFirst

testWavSerde :: TestTree
testWavSerde = testCase "serde" $ do
  -- KnownChunk
  byteSize drumFmtChunk @?= 24
  let fmtBs = encode drumFmtChunk
  (fmt, fmtSz) <- decodeThrow fmtBs
  byteSize fmt @?= fmtSz
  fmt @?= drumFmtChunk
  -- Odd and even unparsed chunks
  let unpSz = 10
      unpOdd = Chunk "FOOB" (UnparsedBody "A")
      unpEven = Chunk "FOOB" (UnparsedBody "AB")
  byteSize unpOdd @?= unpSz
  byteSize unpEven @?= unpSz
  let unpOddBs = encode unpOdd
  (unpOdd', unpOddSz) <- decodeThrow unpOddBs
  byteSize unpOdd' @?= unpOddSz
  unpOdd' @?= unpOdd
  let unpEvenBs = encode unpEven
  (unpEven', unpEvenSz) <- decodeThrow unpEvenBs
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
  (header, bc) <- decodeThrow bs
  header @?= drumHeader
  bc @?= drumFmtOffset

testWavData :: TestTree
testWavData = testCase "data" $ do
  bs <- readShort "testdata/drums.wav"
  (arr, _) <- flip getTargetThrow bs $ do
    getSkip drumDataOffset
    chunk <- get
    case chunk of
      WavChunkData (KnownChunk (WavDataBody (QuietArray arr))) -> pure arr
      _ -> fail "expected data"
  fromIntegral (sizeofByteArray arr) @?= drumDataLen * 2 -- x2 for 2-byte samples

testWavInfo :: TestTree
testWavInfo = testCase "info" $ do
  bs <- readShort "testdata/drums.wav"
  (info, _) <- flip getTargetThrow bs $ do
    getSkip drumPostDataOffset
    chunk <- get
    case chunk of
      WavChunkInfo (KnownListChunk info) -> pure info
      _ -> fail "expected info"
  info @?= Seq.fromList [WavInfoElem "IART" "freewavesamples.com"]

testWavWhole :: TestTree
testWavWhole = testCase "whole" $ do
  bs <- readShort "testdata/drums.wav"
  (wav, _) <- decodeThrow bs
  fmtChunk <- rethrow (guardChunk "format" (lookupWavFormatChunk wav))
  fmtChunk @?= drumFmtChunk
  KnownChunk (WavDataBody (QuietArray arr)) <- rethrow (guardChunk "data" (lookupWavDataChunk wav))
  fromIntegral (sizeofByteArray arr) @?= drumDataLen * 2 -- x2 for 2-byte samples
  Seq.length (wavChunks wav) @?= 4

testWavWrite :: TestTree
testWavWrite = testCase "write" $ do
  bs <- readShort "testdata/drums.wav"
  (wav, bc) <- decodeThrow bs
  byteSize wav @?= bc
  for_ (wavChunks wav) assertReparses
  let bs' = encode wav
  bs' @?= bs

testWavWrite2 :: TestTree
testWavWrite2 = testCase "write" $ do
  bs <- readShort "testdata/DX-EPiano1-C1.wav"
  (wav, bc) <- decodeThrow bs
  byteSize wav @?= bc
  for_ (wavChunks wav) assertReparses
  let bs' = encode wav
  bs' @?= bs

testWav :: TestTree
testWav = testGroup "wav" [testWavSerde, testWavHeader, testWavData, testWavInfo, testWavWhole, testWavWrite, testWavWrite2]

testAiff :: TestTree
testAiff = testCase "aiff" $ do
  bs <- readShort "testdata/M1F1-int16s-AFsp.aif"
  (aiff, bc) <- decodeThrow bs
  byteSize aiff @?= bc
  bc @?= fromIntegral (BSS.length bs)
  for_ (aiffChunks aiff) assertReparses
  let bs' = encode aiff
  fromIntegral (BSS.length bs') @?= bc
  bs' @?= bs

testSfontWhole :: TestTree
testSfontWhole = testCase "whole" $ do
  bs <- readShort "testdata/timpani.sf2"
  (Sfont (InfoChunk (KnownListChunk infos)) (SdtaChunk (KnownOptChunk maySdta)) (PdtaChunk (KnownListChunk pdtaBlocks)), _) <- decodeThrow bs
  Seq.length infos @?= 5
  case maySdta of
    Nothing -> fail "Missing sdta"
    Just sdta -> do
      lengthLiftedPrimArray (sdtaHighBits sdta) @?= 1365026
      sdtaLowBits sdta @?= Nothing
  Seq.length pdtaBlocks @?= 9

testSfontWrite :: TestTree
testSfontWrite = testCase "write" $ do
  bs <- readShort "testdata/timpani.sf2"
  (sfont, _) <- decodeThrow @Sfont bs
  let bs' = encode sfont
  bs' @?= bs

testSfontManual :: TestTree
testSfontManual = testCase "manual" $ do
  bs <- readShort "testdata/timpani.sf2"
  ((info, sdta, pdta), _) <- flip getTargetThrow bs $ do
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
  let infoBs = encode info
  BSS.length infoBs @?= fromIntegral expecInfoSize
  let sdtaBs = encode sdta
  BSS.length sdtaBs @?= fromIntegral expecSdtaSize
  let pdtaBs = encode pdta
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
  (pti, bc) <- decodeThrow bs
  byteSize pti @?= bc
  fromIntegral bc @?= BSS.length bs
  -- divide by two to compare number of elements (2-byte samples)
  lengthLiftedPrimArray (unQuietLiftedArray (ptiPcmData pti)) @?= div drumDataLen 2
  let bs' = encode pti
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
  (actHdr, _) <- decodeThrow bs
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
  ((actHdr, actDigest), _) <- getTargetThrow ((,) <$> get @Header <*> getWord32LE) bs
  let defHdr = def @Header
      actPre = hdrPreamble actHdr
      defPre = hdrPreamble defHdr
      modDefPre = defPre {preName = preName actPre}
      modDefHdr = defHdr {hdrPreamble = modDefPre}
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
  (wav, _) <- decodeThrow wavBs
  (pti, _) <- decodeThrow ptiBs
  KnownChunk (WavDataBody (QuietArray wavData)) <- rethrow (guardChunk "data" (lookupWavDataChunk wav))
  -- We expect there are half as many samples because it's converted to mono
  -- divide by four to compare number of elements (2-byte samples and 2 channels)
  coerce (lengthLiftedPrimArray (unQuietLiftedArray (ptiPcmData pti))) @?= div (sizeofByteArray wavData) 4

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

assertReparses :: (ByteSized a, Binary a, Eq a, Show a) => a -> IO ()
assertReparses a = do
  let !bs = encode a
  (a', bc) <- decodeThrow bs
  byteSize a' @?= bc
  a' @?= a

testConvertDx :: TestTree
testConvertDx = testCase "DX" $ do
  bs <- readShort "testdata/DX-EPiano1-C1.aif"
  (aif, _) <- decodeThrow bs
  ne <- rethrow (aiffToNeutral 44100 aif (Just defaultLoopMarkNames))
  -- Test that a standard translation of the wav works
  let !wav = neutralToWav defaultNoteNumber ne
  pc <- rethrow (wavToPcmContainer wav)
  pc @?= neCon ne
  assertReparses wav
  xne <- rethrow (wavToNeutral wav (Just defaultLoopMarkNames))
  xne @?= ne
  -- Test that the sample wav works
  let !width = 2500 -- double this is 0.1s of total fade
  swav <- rethrow (neutralToSampleWav width defaultNoteNumber ne)
  assertReparses swav

aifSamples :: Aiff -> [Word8]
aifSamples aif =
  let Aiff.KnownChunk (AiffDataBody _ _ (QuietArray wavData)) = fromMaybe (error "no data") (lookupAiffDataChunk aif)
      !sz = sizeofByteArray wavData
  in  fmap (indexByteArray wavData) [0 .. sz - 1]

wavSamples :: Wav -> [Word8]
wavSamples wav =
  let KnownChunk (WavDataBody (QuietArray wavData)) = fromMaybe (error "no data") (lookupWavDataChunk wav)
      !sz = sizeofByteArray wavData
  in  fmap (indexByteArray wavData) [0 .. sz - 1]

neutralSamples :: Neutral -> [Word8]
neutralSamples ne =
  let !wavData = pcData (neCon ne)
      !sz = sizeofByteArray wavData
  in  fmap (indexByteArray wavData) [0 .. sz - 1]

wavFmt :: Wav -> WavFormatBody
wavFmt wav = fmtBody
 where
  KnownChunk fmtBody = fromMaybe (error "no format") (lookupWavFormatChunk wav)

takeN :: Int -> [Word8] -> [Word8]
takeN n xs = let f = take n in f xs ++ reverse (f (reverse xs))

evenShorts :: [Word8] -> [Word8]
evenShorts (x : y : zs) = x : y : oddShorts zs
evenShorts _ = []

oddShorts :: [Word8] -> [Word8]
oddShorts (_ : _ : zs) = evenShorts zs
oddShorts _ = []

takeSome :: [Word8] -> [Word8]
takeSome = takeN 10

takeSomeEvens :: [Word8] -> [Word8]
takeSomeEvens = evenShorts . takeN 20

tryAifToWav :: Int -> String -> IO ()
tryAifToWav channels variant = do
  (aif, _) <- decodeFileThrow ("testdata/sin_" ++ variant ++ ".aifc")
  (wav, _) <- decodeFileThrow ("testdata/sin_" ++ variant ++ ".wav")
  ne <- rethrow (aiffToNeutral 44100 aif Nothing)
  let !conv = neutralToWav defaultNoteNumber ne
  let !aifPart = takeSome (aifSamples aif)
  let !wavPart = takeSome (wavSamples wav)
  wavPart @?= aifPart
  let !convPart = takeSome (wavSamples conv)
  convPart @?= aifPart
  -- conv and wav will be different bc of addl chunks like cue
  -- but their formats will be the same
  let !convFx = wavFmt conv
  wfbNumChannels convFx @?= fromIntegral channels
  let !wavFx = wavFmt wav
  convFx @?= wavFx

tryWavMono :: IO ()
tryWavMono = do
  -- Read stereo + mono versions of same wav, assert their samples are correct
  (wavStereo, _) <- decodeFileThrow "testdata/sin_stereo.wav"
  (wavMono, _) <- decodeFileThrow "testdata/sin_mono.wav"
  let !leftStereoPart = takeSomeEvens (wavSamples wavStereo)
  let !monoPart = takeSome (wavSamples wavMono)
  monoPart @?= leftStereoPart
  -- Now make the stereo wav mono and assert the same
  neExtract <- rethrow (wavToNeutral wavStereo Nothing >>= neutralMono)
  let !extractPart = takeSome (neutralSamples neExtract)
  extractPart @?= monoPart

testConvertSin :: TestTree
testConvertSin = testCase "sin" $ do
  tryAifToWav 1 "mono"
  tryAifToWav 2 "stereo"
  tryWavMono

testConvertLoop :: TestTree
testConvertLoop = testCase "loop" $ do
  (wav, _) <- decodeFileThrow "testdata/sin_mono.wav"
  con <- rethrow (wavToPcmContainer wav)
  let mkLoopMarks = defineLoopMarks @Int defaultLoopMarkNames . fmap (* 6000)
      !loopMarks = mkLoopMarks (LoopMarks 1 2 3 4)
      !marks = Seq.fromList (fmap snd (toList loopMarks))
  let !ne = Neutral {neCon = con, neLoopMarks = Just loopMarks, neMarks = marks}
  -- Convert and write out for inspection
  let !fullWav = neutralToWav defaultNoteNumber ne
  encodeFile fullWav "testoutput/sin_mono_full.wav"
  sampleWav <- rethrow (neutralToSampleWav 2500 defaultNoteNumber ne)
  encodeFile sampleWav "testoutput/sin_mono_sample.wav"
  -- Test full wav marks
  fullNe <- rethrow (wavToNeutral fullWav (Just defaultLoopMarkNames))
  let !actualFullLoopMarks = neLoopMarks fullNe
  actualFullLoopMarks @?= Just loopMarks
  -- Test sample wav marks
  let !expectedSampleLoopMarks = mkLoopMarks (LoopMarks 0 1 2 2)
  sampleNe <- rethrow (wavToNeutral sampleWav (Just defaultLoopMarkNames))
  let !actualSampleLoopMarks = neLoopMarks sampleNe
  actualSampleLoopMarks @?= Just expectedSampleLoopMarks

testConvertFade :: TestTree
testConvertFade = testCase "fade" $ do
  (wav, _) <- decodeFileThrow "testdata/fadeout_post.wav"
  con <- rethrow (wavToPcmContainer wav)
  let mkLoopMarks = defineLoopMarks @Int defaultLoopMarkNames . fmap (* 4410)
      !loopMarks = mkLoopMarks (LoopMarks 0 3 6 10)
      !marks = Seq.fromList (fmap snd (toList loopMarks))
  let !ne = Neutral {neCon = con, neLoopMarks = Just loopMarks, neMarks = marks}
  -- Convert and write out for inspection
  let !fullWav = neutralToWav defaultNoteNumber ne
  encodeFile fullWav "testoutput/fadeout_full.wav"
  sampleWav <- rethrow (neutralToSampleWav 1000 defaultNoteNumber ne)
  encodeFile sampleWav "testoutput/fadeout_sample.wav"

testConvertDxFade :: TestTree
testConvertDxFade = testCase "dx fade" $ do
  (wav, _) <- decodeFileThrow "testdata/DX-EPiano1-C1.wav"
  ne <- rethrow (wavToNeutral wav (Just defaultLoopMarkNames))
  neMon <- rethrow (neutralMono ne)
  -- Convert and write out for inspection
  let !fullWav = neutralToWav defaultNoteNumber neMon
  encodeFile fullWav "testoutput/dx_full.wav"
  sampleWav <- rethrow (neutralToSampleWav 2500 defaultNoteNumber neMon)
  encodeFile sampleWav "testoutput/dx_sample.wav"

testConvert :: TestTree
testConvert = testGroup "convert" [testConvertDx, testConvertSin, testConvertLoop, testConvertFade, testConvertDxFade]

testDspMono :: TestTree
testDspMono = testCase "mono" $ do
  let !larr1 = liftedPrimArrayFromList @Int16LE [0, 1, 2, 3, 4, 5]
      !larr2 = liftedPrimArrayFromList @Int16LE [0, 2, 4]
      !mm1 = ModMeta {mmNumChannels = 2, mmBitsPerSample = 16, mmSampleRate = 44100}
      !mm2 = mm1 {mmNumChannels = 1}
  (!mmx, !larrx) <- rethrow (runMod monoFromLeft mm1 larr1)
  mmx @?= mm2
  larrx @?= larr2

testDspFadeOne :: TestTree
testDspFadeOne = testCase "fade one" $ do
  let !larr1 = liftedPrimArrayFromList @Int8 [5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1]
      --                                                               ^                       ^
      !width = 1
      !loopStart = 7
      !loopEnd = 15
      !larr2 = liftedPrimArrayFromList @Int8 [5, 5, 5, 5, 5, 5, 5, 3, 5, 1, 1, 1, 1, 1, 5, 3, 1]
      --                                                               ^                       ^
      !mm = ModMeta {mmNumChannels = 1, mmBitsPerSample = 8, mmSampleRate = 44100}
  (!mmx, !larrx) <- rethrow (runMod (linearCrossFade width loopStart loopEnd) mm larr1)
  mmx @?= mm
  larrx @?= larr2

testDspFadeSome :: TestTree
testDspFadeSome = testCase "fade some" $ do
  let !larr1 = liftedPrimArrayFromList @Int8 [5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1]
      !width = 3
      !loopStart = 7
      !loopEnd = 15
      !larr2 = liftedPrimArrayFromList @Int8 [5, 5, 5, 5, 5, 4, 3, 3, 3, 1, 1, 1, 5, 4, 3, 3, 2]
      !mm = ModMeta {mmNumChannels = 1, mmBitsPerSample = 8, mmSampleRate = 44100}
  (!mmx, !larrx) <- rethrow (runMod (linearCrossFade width loopStart loopEnd) mm larr1)
  mmx @?= mm
  larrx @?= larr2

testDspFadeWider :: TestTree
testDspFadeWider = testCase "fade wider" $ do
  let !larr1 = liftedPrimArrayFromList @Int16LE [5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1]
      !width = 3
      !loopStart = 7
      !loopEnd = 15
      !larr2 = liftedPrimArrayFromList @Int16LE [5, 5, 5, 5, 5, 4, 3, 3, 3, 1, 1, 1, 5, 4, 3, 3, 2]
      !mm = ModMeta {mmNumChannels = 1, mmBitsPerSample = 16, mmSampleRate = 44100}
  (!mmx, !larrx) <- rethrow (runMod (linearCrossFade width loopStart loopEnd) mm larr1)
  mmx @?= mm
  larrx @?= larr2

testDsp :: TestTree
testDsp = testGroup "dsp" [testDspMono, testDspFadeOne, testDspFadeSome, testDspFadeWider]

testMatchSamples :: TestTree
testMatchSamples = testCase "match samples" $ do
  samps <- matchSamples "DX-EPiano1" "wav" "testdata"
  samps @?= Seq.singleton (Sample "testdata/DX-EPiano1-C1.wav" (OctNote (Octave 1) NoteNameC) Nothing Nothing)
  inst <- defaultInst samps
  Seq.length (isRegions inst) @?= 1

testPatchSfz :: TestTree
testPatchSfz = testCase "sfz" $ do
  sfzContents <- TIO.readFile "testdata/DX-EPiano1.sfz"
  -- test that we can parse and render the sfz
  sfzFile <- either fail pure (parseSfz sfzContents)
  let sfzContents' = renderSfz sfzFile
  sfzFile' <- either fail pure (parseSfz sfzContents')
  sfzFile' @?= sfzFile
  -- test that we can convert it to an instrument
  instDef@(InstDef instControl instSpec) <- either fail pure (sfzToInst sfzFile)
  instControl @?= def
  Seq.length (isRegions instSpec) @?= 1
  sfzFile'' <- either fail pure (instToSfz instDef)
  assertBool "sfz file similarity" (sfzFileSimilar sfzFile'' sfzFile)
  -- test that our conversion is what we expect
  -- NOTE Just use this to update golden output
  -- let outJsonContents = instToJson instDef
  -- TIO.writeFile "testdata/DX-EPiano1.inst.json" outJsonContents
  jsonContents <- TIO.readFile "testdata/DX-EPiano1.inst.json"
  jsonInstDef <- either fail pure (jsonToInst jsonContents)
  instDef @?= jsonInstDef

loadSamp :: Maybe FilePath -> FilePath -> IO PcmContainer
loadSamp mayRelPath fp = do
  let fp' = maybe fp (</> fp) mayRelPath
  ne <- loadNeutral 44100 (Just defaultLoopMarkNames) fp'
  ne' <- rethrow (neutralMono ne)
  pure $! neCon ne'

testPatchPti :: TestTree
testPatchPti = testCase "pti" $ do
  jsonContents <- TIO.readFile "testdata/DX-EPiano1.inst.json"
  InstDef instControl fileSpec <- either fail pure (jsonToInst jsonContents)
  instControl @?= def
  loadedSpec <- traverse (loadSamp (Just "testdata")) fileSpec
  patches <- either fail pure (instToPtiPatches "DX-EPiano1" Nothing def loadedSpec)
  Seq.length patches @?= 1
  (expectedPti, _) <- decodeFileThrow "testdata/DX-EPiano1-C1.pti"
  patches @?= Seq.singleton (PtiPatch "DX-EPiano1-C1" (InstKeyRange 0 24 127) expectedPti)

testPatches :: TestTree
testPatches = testGroup "patches" [testMatchSamples, testPatchSfz, testPatchPti]

testScrapti :: TestTree
testScrapti = testGroup "Scrapti" [testWav, testAiff, testSfont, testPti, testProject, testConvert, testDsp, testOtherSizes, testPatches]

main :: IO ()
main = defaultMain testScrapti
