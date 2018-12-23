module D22P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D22
import D22P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D22P2.minminutes: gives correct answer to the original problem" $ do
            45 @=? minminutes 510 (10, 10)
    ]