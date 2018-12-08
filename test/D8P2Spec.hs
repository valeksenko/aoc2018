module D8P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D8P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D8P2.datavalue: gives correct answer to the original problem" $ do
            66 @=? D8P2.datavalue [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
    ]
