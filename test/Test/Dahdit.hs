module Test.Dahdit (testDahdit) where

import Dahdit (Binary (..), ByteCount, ByteSized (..), FloatLE (..), Generic, Get, Int16LE, Int32LE, Int8, Proxy (..),
               Put, ShortByteString, StaticByteSized (..), ViaGeneric (..), ViaStaticGeneric (..), Word16LE, Word32LE,
               Word8, getByteString, getFloatLE, getInt16LE, getInt32LE, getInt8, getSeq, getStaticArray, getStaticSeq,
               getWord16LE, getWord32LE, getWord8, putByteString, putFloatLE, putInt16LE, putInt32LE, putInt8, putSeq,
               putStaticArray, putStaticSeq, putWord16LE, putWord32LE, putWord8, runCount, runGet, runPut)
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

runGetCase :: (Show a, Eq a) => Get a -> Maybe (ByteCount, a) -> [Word8] -> IO ()
runGetCase getter mayRes bsl = do
  let bs = BSS.pack bsl
      (result, actBc) = runGet getter bs
  case (result, mayRes) of
    (Left _, Nothing) -> pure ()
    (Left err, Just (_, expecVal)) -> fail ("Got error <" ++ show err ++ ">, expected value <" ++ show expecVal ++ ">")
    (Right actVal, Nothing) -> fail ("Got value <" ++ show actVal ++ ">, expected error")
    (Right actVal, Just (expecBc, expecVal)) -> do
      actVal @?= expecVal
      actBc @?= expecBc

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
  ]

testDahditGet :: TestTree
testDahditGet = testGroup "get"
  [ testCase "Word8 zero" (runGetCase getWord8 Nothing [])
  , testCase "Word8 one" (runGetCase getWord8 (Just (1, 0x5D)) [0x5D])
  , testCase "Word8 two" (runGetCase getWord8 (Just (1, 0x5D)) [0x5D, 0xBB])
  , testCase "Int8" (runGetCase getInt8 (Just (1, 0x5D)) [0x5D])
  , testCase "Word16LE zero" (runGetCase getWord16LE Nothing [])
  , testCase "Word16LE one" (runGetCase getWord16LE Nothing [0x5D])
  , testCase "Word16LE two" (runGetCase getWord16LE (Just (2, 0x5DEC)) [0xEC, 0x5D])
  , testCase "Word16LE three" (runGetCase getWord16LE (Just (2, 0x5DEC)) [0xEC, 0x5D, 0xBB])
  , testCase "Int16LE" (runGetCase getInt16LE (Just (2, 0x5DEC)) [0xEC, 0x5D, 0xBB])
  , testCase "Word32LE" (runGetCase getWord32LE (Just (4, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
  , testCase "Int32LE" (runGetCase getInt32LE (Just (4, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
  , testCase "FloatLE" (runGetCase getFloatLE (Just (4, FloatLE (castWord32ToFloat 0x5DEC6EFD))) [0xFD, 0x6E, 0xEC, 0x5D])
  , testCase "ShortByteString" (runPutCase (putByteString (BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D])
  , testCase "ShortByteString" (runGetCase (getByteString 2) (Just (2, BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D, 0xBB])
  , testCase "Two Word8" (runGetCase ((,) <$> getWord8 <*> getWord8) (Just (2, (0x5D, 0xBB))) [0x5D, 0xBB])
  , testCase "Two Word16LE" (runGetCase ((,) <$> getWord16LE <*> getWord16LE) (Just (4, (0x5DEC, 0x4020))) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "Seq" (runGetCase (getSeq 2 getWord16LE) (Just (4, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticSeq" (runGetCase (getStaticSeq 2 getWord16LE) (Just (4, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticArray" (runGetCase (getStaticArray @Word16LE 2) (Just (4, primArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "DynFoo" (runGetCase (get @DynFoo) (Just (3, DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
  , testCase "StaFoo" (runGetCase (get @StaFoo) (Just (3, StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
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
  ]

testDahdit :: TestTree
testDahdit = testGroup "Dahdit" [ testDahditByteSize, testDahditStaticByteSize, testDahditGet, testDahditPut ]
