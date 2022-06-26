module Test.Dahdit (testDahdit) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

testDahditBasic :: TestTree
testDahditBasic = testCase "basic" $ do
  pure ()

testDahdit :: TestTree
testDahdit = testGroup "Dahdit" [ testDahditBasic ]

