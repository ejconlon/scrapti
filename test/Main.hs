module Main (main) where

import Test.Dahdit (testDahdit)
import Test.Scrapti (testScrapti)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "Main" [testDahdit, testScrapti])
