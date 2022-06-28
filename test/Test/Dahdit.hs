module Test.Dahdit (testDahdit) where

import Dahdit (Binary (..), BoolByte (BoolByte), ByteCount, ByteSized (..), FloatLE (..), Generic, Get, Int16LE,
               Int32LE, Int8, Proxy (..), Put, ShortByteString, StaticByteSized (..), ViaGeneric (..),
               ViaStaticGeneric (..), Word16LE, Word32LE, Word8, getByteString, getExact, getFloatLE, getInt16LE,
               getInt32LE, getInt8, getLookAhead, getRemainingSize, getSeq, getSkip, getStaticArray, getStaticSeq,
               getWithin, getWord16LE, getWord32LE, getWord8, putByteString, putFloatLE, putInt16LE, putInt32LE,
               putInt8, putSeq, putStaticArray, putStaticSeq, putWord16LE, putWord32LE, putWord8, runCount, runGet,
               runPut)
import qualified Data.ByteString.Short as BSS
import Data.Primitive.PrimArray (primArrayFromList)
import qualified Data.Sequence as Seq
import GHC.Float (castWord32ToFloat)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data DynFoo = DynFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, Binary) via (ViaGeneric DynFoo)

data StaFoo = StaFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric StaFoo)

runGetCase :: (Show a, Eq a) => Get a -> Maybe (ByteCount, ByteCount, a) -> [Word8] -> IO ()
runGetCase getter mayRes bsl = do
  let bs = BSS.pack bsl
      (result, actBc) = runGet getter bs
  case (result, mayRes) of
    (Left _, Nothing) -> pure ()
    (Left err, Just (_, _, expecVal)) -> fail ("Got error <" ++ show err ++ ">, expected value <" ++ show expecVal ++ ">")
    (Right actVal, Nothing) -> fail ("Got value <" ++ show actVal ++ ">, expected error")
    (Right actVal, Just (expecBc, expecLeft, expecVal)) -> do
      actVal @?= expecVal
      actBc @?= expecBc
      BSS.length bs - fromIntegral actBc @?= fromIntegral expecLeft

runPutCase :: Put -> [Word8] -> IO ()
runPutCase putter expecList = do
  let expecBc = fromIntegral (length expecList)
      expecBs = BSS.pack expecList
      estBc = runCount putter
  estBc @?= expecBc
  let actBs = runPut putter
      actBc = byteSize actBs
  actBs @?= expecBs
  actBc @?= expecBc

testDahditByteSize :: TestTree
testDahditByteSize = testGroup "byteSize"
  [ testCase "Word8" (byteSize @Word8 0x5D @?= 1)
  , testCase "Int8" (byteSize @Int8 0x5D @?= 1)
  , testCase "Word16LE" (byteSize @Word16LE 0x5DEC @?= 2)
  , testCase "Int16LE" (byteSize @Int16LE 0x5DEC @?= 2)
  , testCase "Word32LE" (byteSize @Word32LE 0x5DEC6EFD @?= 4)
  , testCase "Int32LE" (byteSize @Int32LE 0x5DEC6EFD @?= 4)
  , testCase "FloatLE" (byteSize (FloatLE (castWord32ToFloat 0x5DEC6EFD)) @?= 4)
  , testCase "ShortByteString" (byteSize @ShortByteString (BSS.pack [0xEC, 0x5D]) @?= 2)
  , testCase "DynFoo" (byteSize (DynFoo 0xBB 0x5DEC) @?= 3)
  , testCase "StaFoo" (byteSize (StaFoo 0xBB 0x5DEC) @?= 3)
  ]

testDahditStaticByteSize :: TestTree
testDahditStaticByteSize = testGroup "staticByteSize"
  [ testCase "Word8" (staticByteSize @Word8 Proxy @?= 1)
  , testCase "Int8" (staticByteSize @Int8 Proxy @?= 1)
  , testCase "Word16LE" (staticByteSize @Word16LE Proxy @?= 2)
  , testCase "Int16LE" (staticByteSize @Int16LE Proxy @?= 2)
  , testCase "Word32LE" (staticByteSize @Word32LE Proxy @?= 4)
  , testCase "Int32LE" (staticByteSize @Int32LE Proxy @?= 4)
  , testCase "FloatLE" (staticByteSize @FloatLE Proxy @?= 4)
  , testCase "StaFoo" (staticByteSize @StaFoo Proxy @?= 3)
  , testCase "BoolByte" (staticByteSize @BoolByte Proxy @?= 1)
  ]

testDahditGet :: TestTree
testDahditGet = testGroup "get"
  [ testCase "Word8 zero" (runGetCase getWord8 Nothing [])
  , testCase "Word8 one" (runGetCase getWord8 (Just (1, 0, 0x5D)) [0x5D])
  , testCase "Word8 two" (runGetCase getWord8 (Just (1, 1, 0x5D)) [0x5D, 0xBB])
  , testCase "Int8" (runGetCase getInt8 (Just (1, 0, 0x5D)) [0x5D])
  , testCase "Word16LE zero" (runGetCase getWord16LE Nothing [])
  , testCase "Word16LE one" (runGetCase getWord16LE Nothing [0x5D])
  , testCase "Word16LE two" (runGetCase getWord16LE (Just (2, 0, 0x5DEC)) [0xEC, 0x5D])
  , testCase "Word16LE three" (runGetCase getWord16LE (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
  , testCase "Int16LE" (runGetCase getInt16LE (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
  , testCase "Word32LE" (runGetCase getWord32LE (Just (4, 0, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
  , testCase "Int32LE" (runGetCase getInt32LE (Just (4, 0, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
  , testCase "FloatLE" (runGetCase getFloatLE (Just (4, 0, FloatLE (castWord32ToFloat 0x5DEC6EFD))) [0xFD, 0x6E, 0xEC, 0x5D])
  , testCase "ShortByteString" (runGetCase (getByteString 2) (Just (2, 1, BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D, 0xBB])
  , testCase "Two Word8" (runGetCase ((,) <$> getWord8 <*> getWord8) (Just (2, 0, (0x5D, 0xBB))) [0x5D, 0xBB])
  , testCase "Two Word16LE" (runGetCase ((,) <$> getWord16LE <*> getWord16LE) (Just (4, 0, (0x5DEC, 0x4020))) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "Seq" (runGetCase (getSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticSeq" (runGetCase (getStaticSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticArray" (runGetCase (getStaticArray @Word16LE 2) (Just (4, 0, primArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "DynFoo" (runGetCase (get @DynFoo) (Just (3, 0, DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
  , testCase "StaFoo" (runGetCase (get @StaFoo) (Just (3, 0, StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
  , testCase "getRemainingSize" (runGetCase getRemainingSize (Just (0, 3, 3)) [0xBB, 0xEC, 0x5D])
  , testCase "getSkip" (runGetCase (getSkip 2) (Just (2, 1, ())) [0xBB, 0xEC, 0x5D])
  , testCase "getLookAhead" (runGetCase (getLookAhead getWord16LE) (Just (0, 3, 0x5DEC)) [0xEC, 0x5D, 0xBB])
  , testCase "getExact eq" (runGetCase (getExact 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
  , testCase "getExact lt" (runGetCase (getExact 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB])
  , testCase "getExact gt" (runGetCase (getExact 3 getWord16LE) Nothing [0xEC, 0x5D, 0xBB])
  , testCase "getWithin eq" (runGetCase (getWithin 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
  , testCase "getWithin lt" (runGetCase (getWithin 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB])
  , testCase "getWithin gt" (runGetCase (getWithin 3 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
  , testCase "BoolByte True" (runGetCase (get @BoolByte) (Just (1, 0, BoolByte True)) [0x01])
  , testCase "BoolByte False" (runGetCase (get @BoolByte) (Just (1, 0, BoolByte False)) [0x00])
  ]

testDahditPut :: TestTree
testDahditPut = testGroup "put"
  [ testCase "Word8" (runPutCase (putWord8 0x5D) [0x5D])
  , testCase "Int8" (runPutCase (putInt8 0x5D) [0x5D])
  , testCase "Word16LE" (runPutCase (putWord16LE 0x5DEC) [0xEC, 0x5D])
  , testCase "Int16LE" (runPutCase (putInt16LE 0x5DEC) [0xEC, 0x5D])
  , testCase "Word32LE" (runPutCase (putWord32LE 0x5DEC6EFD) [0xFD, 0x6E, 0xEC, 0x5D])
  , testCase "Int32LE" (runPutCase (putInt32LE 0x5DEC6EFD) [0xFD, 0x6E, 0xEC, 0x5D])
  , testCase "FloatLE" (runPutCase (putFloatLE (FloatLE (castWord32ToFloat 0x5DEC6EFD))) [0xFD, 0x6E, 0xEC, 0x5D])
  , testCase "ShortByteString" (runPutCase (putByteString (BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D])
  , testCase "Two Word8" (runPutCase (putWord8 0x5D *> putWord8 0xBB) [0x5D, 0xBB])
  , testCase "Two Word16LE" (runPutCase (putWord16LE 0x5DEC *> putWord16LE 0x4020) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "Seq" (runPutCase (putSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticSeq" (runPutCase (putStaticSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticArray" (runPutCase (putStaticArray @Word16LE (primArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "DynFoo" (runPutCase (put (DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
  , testCase "StaFoo" (runPutCase (put (StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
  , testCase "BoolByte True" (runPutCase (put (BoolByte True)) [0x01])
  , testCase "BoolByte False" (runPutCase (put (BoolByte False)) [0x00])
  ]

testDahdit :: TestTree
testDahdit = testGroup "Dahdit" [ testDahditByteSize, testDahditStaticByteSize, testDahditGet, testDahditPut ]
