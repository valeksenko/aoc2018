module D22P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D22P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D22P1.risklevel: gives correct answer to the original problem" $ do
            114 @=? risklevel 510 (10, 10)
    ]