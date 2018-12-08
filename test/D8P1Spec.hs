module D8P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D8P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D8P1.sumdata: gives correct answer to the original problem" $ do
            138 @=? D8P1.sumdata [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
    ]
