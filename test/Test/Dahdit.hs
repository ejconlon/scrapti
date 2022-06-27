module Test.Dahdit where -- (testDahdit) where

import Dahdit.Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.Primitive.ByteArray (byteArrayFromList)
import Data.Primitive.PrimArray (primArrayFromList)
import qualified Data.Sequence as Seq
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
  let bs = BS.pack bsl
      (result, actBc, _) = runGetBS getter bs
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
      expecArr = byteArrayFromList expecList
      estBc = runCount putter
  estBc @?= expecBc
  let (actBc, actArr) = runPutArray putter
  actBc @?= expecBc
  actArr @?= expecArr

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
  , testCase "ByteString" (runGetCase (getShortByteString 2) (Just (2, BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D, 0xBB])
  , testCase "Two Word8" (runGetCase ((,) <$> getWord8 <*> getWord8) (Just (2, (0x5D, 0xBB))) [0x5D, 0xBB])
  , testCase "Two Word16LE" (runGetCase ((,) <$> getWord16LE <*> getWord16LE) (Just (4, (0x5DEC, 0x4020))) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "Seq" (runGetCase (getSeq 2 getWord16LE) (Just (4, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticSeq" (runGetCase (getStaticSeq 2 getWord16LE) (Just (4, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticArray" (runGetCase (getStaticArray @Word16LE 2) (Just (4, primArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  ]

testDahditPut :: TestTree
testDahditPut = testGroup "put"
  [ testCase "Word8" (runPutCase (putWord8 0x5D) [0x5D])
  , testCase "Int8" (runPutCase (putInt8 0x5D) [0x5D])
  , testCase "Word16LE" (runPutCase (putWord16LE 0x5DEC) [0xEC, 0x5D])
  , testCase "Int16LE" (runPutCase (putInt16LE 0x5DEC) [0xEC, 0x5D])
  , testCase "ByteString" (runPutCase (putShortByteString (BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D])
  , testCase "Two Word8" (runPutCase (putWord8 0x5D *> putWord8 0xBB) [0x5D, 0xBB])
  , testCase "Two Word16LE" (runPutCase (putWord16LE 0x5DEC *> putWord16LE 0x4020) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "Seq" (runPutCase (putSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticSeq" (runPutCase (putStaticSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  , testCase "StaticArray" (runPutCase (putStaticArray @Word16LE (primArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
  ]

testDahdit :: TestTree
testDahdit = testGroup "Dahdit" [ testDahditGet, testDahditPut ]
