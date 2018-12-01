module D1P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D1P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D1P1.frequency: gives correct answer to the original problem" $ do
            3 @=? D1P1.frequency [1, 1, 1]
            0 @=? D1P1.frequency [1, 1, -2]
            -6 @=? D1P1.frequency [-1, -2, -3]
    ]