module Main (main) where

import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified Reader.Test as Reader (tests)

tests :: IO TestTree
tests = testGroup "Main" <$> sequence [ Reader.tests ]

main :: IO ()
main = defaultMain =<< tests
