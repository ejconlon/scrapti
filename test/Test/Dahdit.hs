module Test.Dahdit where -- (testDahdit) where

import Dahdit.Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Sequence as Seq
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

runGetCase :: (Show a, Eq a) => Get a -> Maybe (ByteCount, a) -> ByteString -> IO ()
runGetCase getter mayRes bs = do
  let (result, actBc, _) = runGetBS getter bs
  -- putStrLn ("<<<<" ++ show (result, bc) ++ ">>>>")
  case (result, mayRes) of
    (Left _, Nothing) -> pure ()
    (Left err, Just (_, expecVal)) -> fail ("Got error <" ++ show err ++ ">, expected value <" ++ show expecVal ++ ">")
    (Right actVal, Nothing) -> fail ("Got value <" ++ show actVal ++ ">, expected error")
    (Right actVal, Just (expecBc, expecVal)) -> do
      actVal @?= expecVal
      actBc @?= expecBc
  -- bs' @?= BS.drop (fromIntegral bc) bs

testDahditGet :: TestTree
testDahditGet = testGroup "get"
  [ testCase "Word8 zero" (runGetCase getWord8 Nothing BS.empty)
  , testCase "Word8 one" (runGetCase getWord8 (Just (1, 0x5D)) (BS.pack [0x5D]))
  , testCase "Word8 two" (runGetCase getWord8 (Just (1, 0x5D)) (BS.pack [0x5D, 0xBB]))
  , testCase "Int8" (runGetCase getInt8 (Just (1, 0x5D)) (BS.pack [0x5D]))
  , testCase "Word16LE zero" (runGetCase getWord16LE Nothing BS.empty)
  , testCase "Word16LE one" (runGetCase getWord16LE Nothing (BS.pack [0x5D]))
  , testCase "Word16LE two" (runGetCase getWord16LE (Just (2, 0x5DEC)) (BS.pack [0xEC, 0x5D]))
  , testCase "Word16LE three" (runGetCase getWord16LE (Just (2, 0x5DEC)) (BS.pack [0xEC, 0x5D, 0xBB]))
  , testCase "Int16LE" (runGetCase getInt16LE (Just (2, 0x5DEC)) (BS.pack [0xEC, 0x5D, 0xBB]))
  , testCase "ByteString" (runGetCase (getShortByteString 2) (Just (2, BSS.pack [0xEC, 0x5D])) (BS.pack [0xEC, 0x5D, 0xBB]))
  , testCase "Two Word8" (runGetCase ((,) <$> getWord8 <*> getWord8) (Just (2, (0x5D, 0xBB))) (BS.pack [0x5D, 0xBB]))
  , testCase "Two Word16LE" (runGetCase ((,) <$> getWord16LE <*> getWord16LE) (Just (4, (0x5DEC, 0x4020))) (BS.pack [0xEC, 0x5D, 0x20, 0x40]))
  , testCase "Seq" (runGetCase (getSeq 2 getWord16LE) (Just (4, Seq.fromList [0x5DEC, 0x4020])) (BS.pack [0xEC, 0x5D, 0x20, 0x40]))
  , testCase "StaticSeq" (runGetCase (getStaticSeq 2 getWord16LE) (Just (4, Seq.fromList [0x5DEC, 0x4020])) (BS.pack [0xEC, 0x5D, 0x20, 0x40]))
  ]

testDahdit :: TestTree
testDahdit = testGroup "Dahdit" [ testDahditGet ]
