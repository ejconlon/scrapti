module Test.Dahdit where -- (testDahdit) where

import Dahdit.Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

runGetCase :: (ByteSized a, Show a, Eq a) => Get a -> Maybe a -> ByteString -> IO ()
runGetCase getter mayValue bs = do
  let (result, bc, _) = runGet getter bs
  -- putStrLn ("<<<<" ++ show (result, bc) ++ ">>>>")
  case (result, mayValue) of
    (Left _, Nothing) -> pure ()
    (Left err, Just expec) -> fail ("Got error <" ++ show err ++ ">, expected value <" ++ show expec ++ ">")
    (Right act, Nothing) -> fail ("Got value <" ++ show act ++ ">, expected error")
    (Right act, Just expec) -> do
      act @?= expec
      byteSize act @?= bc
  -- bs' @?= BS.drop (fromIntegral bc) bs

testDahditGet :: TestTree
testDahditGet = testGroup "get"
  [ testCase "Word8 zero" (runGetCase getWord8 Nothing BS.empty)
  , testCase "Word8 one" (runGetCase getWord8 (Just 0x5D) (BS.pack [0x5D]))
  , testCase "Word8 two" (runGetCase getWord8 (Just 0x5D) (BS.pack [0x5D, 0xBB]))
  , testCase "Int8" (runGetCase getInt8 (Just 0x5D) (BS.pack [0x5D]))
  , testCase "Word16LE zero" (runGetCase getWord16LE Nothing BS.empty)
  , testCase "Word16LE one" (runGetCase getWord16LE Nothing (BS.pack [0x5D]))
  , testCase "Word16LE two" (runGetCase getWord16LE (Just 0x5DEC) (BS.pack [0xEC, 0x5D]))
  , testCase "Word16LE three" (runGetCase getWord16LE (Just 0x5DEC) (BS.pack [0xEC, 0x5D, 0xBB]))
  , testCase "Int16LE" (runGetCase getInt16LE (Just 0x5DEC) (BS.pack [0xEC, 0x5D, 0xBB]))
  , testCase "ByteString" (runGetCase (getByteString 2) (Just (BSS.pack [0xEC, 0x5D])) (BS.pack [0xEC, 0x5D, 0xBB]))
  ]

testDahdit :: TestTree
testDahdit = testGroup "Dahdit" [ testDahditGet ]
